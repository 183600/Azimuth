// 标准增强测试用例 - 覆盖新的测试场景
test "mathematical_identity_properties" {
  // 测试数学恒等性质
  
  // 加法恒等元
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  
  // 乘法恒等元
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(1, -17))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -17))
}

test "divide_with_ceil_negative_divisors" {
  // 测试负数除数的向上取整除法
  
  // 正数除以负数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(6, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(2, -3))
  
  // 负数除以负数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-6, -3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-2, -3))
  
  // 零除以负数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
}

test "string_greeting_multilingual_support" {
  // 测试多语言问候支持
  
  // 中文
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  
  // 日文
  @azimuth.assert_eq_string("Hello, こんにちは!", @azimuth.greet("こんにちは"))
  @azimuth.assert_eq_string("Hello, さようなら!", @azimuth.greet("さようなら"))
  
  // 韩文
  @azimuth.assert_eq_string("Hello, 안녕하세요!", @azimuth.greet("안녕하세요"))
  
  // 阿拉伯文
  @azimuth.assert_eq_string("Hello, مرحبا!", @azimuth.greet("مرحبا"))
  
  // 俄文
  @azimuth.assert_eq_string("Hello, Привет!", @azimuth.greet("Привет"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  
  // 场景1：计算复合利息（简化版）
  let principal = 1000
  let rate1 = 5
  let rate2 = 3
  let rate3 = 2
  
  let year1 = @azimuth.multiply(principal, rate1)
  let year2 = @azimuth.multiply(year1, rate2)
  let year3 = @azimuth.multiply(year2, rate3)
  @azimuth.assert_eq(30000, year3)
  
  // 场景2：计算平均数（使用加法和除法）
  let scores = @azimuth.add(@azimuth.add(85, 90), 95)
  let average = @azimuth.divide_with_ceil(scores, 3)
  @azimuth.assert_eq(90, average)
  
  // 场景3：计算面积和周长
  let length = 8
  let width = 6
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.add(length, width), @azimuth.add(length, width))
  @azimuth.assert_eq(48, area)
  @azimuth.assert_eq(28, perimeter)
}

test "edge_case_number_sequences" {
  // 测试边界情况的数字序列
  
  // 斐波那契数列前几项（简化计算）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
  
  // 2的幂次方
  let pow2_1 = 2
  let pow2_2 = @azimuth.multiply(pow2_1, 2)  // 4
  let pow2_3 = @azimuth.multiply(pow2_2, 2)  // 8
  let pow2_4 = @azimuth.multiply(pow2_3, 2)  // 16
  let pow2_5 = @azimuth.multiply(pow2_4, 2)  // 32
  
  @azimuth.assert_eq(4, pow2_2)
  @azimuth.assert_eq(8, pow2_3)
  @azimuth.assert_eq(16, pow2_4)
  @azimuth.assert_eq(32, pow2_5)
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  
  // 接近整数的情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(13, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(14, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  
  // 大数除法
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(500000, @azimuth.divide_with_ceil(1000000, 2))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
  @azimuth.assert_eq(250000, @azimuth.divide_with_ceil(1000000, 4))
  
  // 极小数除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000))
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  
  // 除零处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 极值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "performance_and_stress_testing" {
  // 测试性能和压力
  
  // 连续运算压力测试
  let mut result = 1
  result = @azimuth.add(result, 1)  // 2
  result = @azimuth.multiply(result, 2)  // 4
  result = @azimuth.add(result, 1)  // 5
  result = @azimuth.multiply(result, 2)  // 10
  result = @azimuth.add(result, 1)  // 11
  result = @azimuth.multiply(result, 2)  // 22
  result = @azimuth.add(result, 1)  // 23
  result = @azimuth.multiply(result, 2)  // 46
  
  @azimuth.assert_eq(46, result)
  
  // 嵌套运算压力测试
  let nested1 = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4)
  let nested2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 3), 4), 5)
  let nested3 = @azimuth.divide_with_ceil(@azimuth.divide_with_ceil(100, 3), 2)
  
  @azimuth.assert_eq(10, nested1)
  @azimuth.assert_eq(120, nested2)
  @azimuth.assert_eq(17, nested3)
}

test "real_world_application_testing" {
  // 测试实际应用场景
  
  // 场景1：计算订单总价（含税费）
  let item_price = 199
  let quantity = 3
  let subtotal = @azimuth.multiply(item_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, 8), 100)  // 8%税费
  let total = @azimuth.add(subtotal, tax)
  
  @azimuth.assert_eq(597, subtotal)
  @azimuth.assert_eq(48, tax)
  @azimuth.assert_eq(645, total)
  
  // 场景2：计算项目完成时间
  let days_per_task = 5
  let number_of_tasks = 8
  let buffer_days = 10
  let total_days = @azimuth.add(@azimuth.multiply(days_per_task, number_of_tasks), buffer_days)
  
  @azimuth.assert_eq(50, total_days)
  
  // 场景3：生成批量个性化消息
  let user_names = ["Alice", "Bob", "Charlie"]
  let greeting1 = @azimuth.greet(user_names[0])
  let greeting2 = @azimuth.greet(user_names[1])
  let greeting3 = @azimuth.greet(user_names[2])
  
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  @azimuth.assert_eq_string("Hello, Charlie!", greeting3)
}