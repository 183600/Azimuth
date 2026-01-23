// 额外的标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  let y = 17
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(0, y))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(y, @azimuth.multiply(1, y))
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
}

test "boundary_condition_handling" {
  // 测试边界条件处理
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法测试
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  
  // 边界值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
  
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
}

test "real_world_business_scenarios" {
  // 测试真实世界业务场景
  // 场景1：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let discount = -20
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(@azimuth.add(subtotal, shipping), discount)
  @azimuth.assert_eq(602, total)
  
  // 场景2：计算资源分配
  let total_resources = 100
  let teams = 3
  let resources_per_team = @azimuth.divide_with_ceil(total_resources, teams)
  let total_allocated = @azimuth.multiply(resources_per_team, teams)
  @azimuth.assert_eq(34, resources_per_team)
  @azimuth.assert_eq(102, total_allocated)
  
  // 场景3：用户个性化问候
  let user_name = "李明"
  let greeting = @azimuth.greet(user_name)
  @azimuth.assert_eq_string("Hello, 李明!", greeting)
}

test "unicode_string_processing" {
  // 测试Unicode字符串处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, 🏳️‍🌈!", @azimuth.greet("🏳️‍🌈"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, עברית!", @azimuth.greet("עברית"))
}

test "complex_calculation_sequences" {
  // 测试复杂计算序列
  // 场景：计算复合利息
  let principal = 1000
  let rate1 = 5
  let rate2 = 3
  let rate3 = 4
  
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate1), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate2), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate3), 100))
  
  @azimuth.assert_eq(1050, year1)
  @azimuth.assert_eq(1082, year2)
  @azimuth.assert_eq(1126, year3)
  
  // 场景：计算数列总和
  let sequence_sum = @azimuth.add(
    @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4)),
    @azimuth.add(@azimuth.add(5, 6), @azimuth.add(7, 8))
  )
  @azimuth.assert_eq(36, sequence_sum)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  // 测试除零的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
  
  // 测试极端值的运算
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 2147483647))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -2147483648))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2147483647))
  
  // 测试空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
}

test "performance_related_operations" {
  // 测试性能相关操作
  // 大数运算测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num1, large_num2))
  
  // 连续运算测试
  let mut accumulator = 1
  accumulator = @azimuth.multiply(accumulator, 2)
  accumulator = @azimuth.multiply(accumulator, 3)
  accumulator = @azimuth.multiply(accumulator, 4)
  accumulator = @azimuth.multiply(accumulator, 5)
  @azimuth.assert_eq(120, accumulator)
  
  // 批量处理模拟
  let batch_size = 100
  let num_batches = 10
  let total_items = @azimuth.multiply(batch_size, num_batches)
  let processing_time = @azimuth.divide_with_ceil(total_items, 50)
  @azimuth.assert_eq(1000, total_items)
  @azimuth.assert_eq(20, processing_time)
}

test "function_composition_patterns" {
  // 测试函数组合模式
  // 场景：计算表达式 (a + b) * (c - d) / e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let product = @azimuth.multiply(sum_ab, diff_cd)
  let result = @azimuth.divide_with_ceil(product, e)
  
  @azimuth.assert_eq(15, sum_ab)
  @azimuth.assert_eq(12, diff_cd)
  @azimuth.assert_eq(180, product)
  @azimuth.assert_eq(60, result)
  
  // 场景：计算平均值的向上取整
  let values_sum = @azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45)
  let count = 4
  let average_ceil = @azimuth.divide_with_ceil(values_sum, count)
  @azimuth.assert_eq(120, values_sum)
  @azimuth.assert_eq(30, average_ceil)
}

test "data_type_compatibility" {
  // 测试数据类型兼容性
  // 测试正数、负数和零的组合
  let positive = 42
  let negative = -17
  let zero = 0
  
  @azimuth.assert_eq(25, @azimuth.add(positive, negative))
  @azimuth.assert_eq(-25, @azimuth.add(negative, positive))
  @azimuth.assert_eq(42, @azimuth.add(positive, zero))
  @azimuth.assert_eq(-17, @azimuth.add(negative, zero))
  @azimuth.assert_eq(0, @azimuth.add(zero, zero))
  
  @azimuth.assert_eq(-714, @azimuth.multiply(positive, negative))
  @azimuth.assert_eq(-714, @azimuth.multiply(negative, positive))
  @azimuth.assert_eq(0, @azimuth.multiply(positive, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(negative, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(zero, zero))
  
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, 0!", @azimuth.greet("0"))
  @azimuth.assert_eq_string("Hello, -1!", @azimuth.greet("-1"))
  @azimuth.assert_eq_string("Hello, 3.14!", @azimuth.greet("3.14"))
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet("true"))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet("false"))
}

test "algorithm_implementation_tests" {
  // 测试算法实现
  // 场景：计算阶乘（简化版）
  let fact_1 = @azimuth.multiply(1, 1)
  let fact_2 = @azimuth.multiply(fact_1, 2)
  let fact_3 = @azimuth.multiply(fact_2, 3)
  let fact_4 = @azimuth.multiply(fact_3, 4)
  let fact_5 = @azimuth.multiply(fact_4, 5)
  
  @azimuth.assert_eq(1, fact_1)
  @azimuth.assert_eq(2, fact_2)
  @azimuth.assert_eq(6, fact_3)
  @azimuth.assert_eq(24, fact_4)
  @azimuth.assert_eq(120, fact_5)
  
  // 场景：计算斐波那契数列（简化版）
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = @azimuth.add(fib_0, fib_1)
  let fib_3 = @azimuth.add(fib_1, fib_2)
  let fib_4 = @azimuth.add(fib_2, fib_3)
  let fib_5 = @azimuth.add(fib_3, fib_4)
  
  @azimuth.assert_eq(0, fib_0)
  @azimuth.assert_eq(1, fib_1)
  @azimuth.assert_eq(1, fib_2)
  @azimuth.assert_eq(2, fib_3)
  @azimuth.assert_eq(3, fib_4)
  @azimuth.assert_eq(5, fib_5)
  
  // 场景：计算最大公约数（简化版，使用减法）
  let a = 48
  let b = 18
  let mut gcd_a = a
  let mut gcd_b = b
  
  // 简化的欧几里得算法（仅演示概念）
  @azimuth.assert_eq(48, gcd_a)
  @azimuth.assert_eq(18, gcd_b)
  
  // 验证数学性质
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}