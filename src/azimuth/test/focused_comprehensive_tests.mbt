// 专注的综合测试用例 - 补充现有测试覆盖的场景

// 测试用例1: 复合边界条件测试
///|
test "composite_boundary_conditions" {
  // 测试多个边界条件的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算组合
  assert_eq(-1, @azimuth.add(max_val, min_val))
  assert_eq(-1, @azimuth.add(min_val, max_val))
  
  // 接近边界的值相加
  assert_eq(max_val, @azimuth.add(1073741823, 1073741824))
  assert_eq(min_val, @azimuth.add(-1073741824, -1073741824))
  
  // 边界值的乘法组合
  assert_eq(min_val, @azimuth.multiply(min_val, 1))
  assert_eq(min_val, @azimuth.multiply(1, min_val))
  assert_eq(0, @azimuth.multiply(min_val, 0))
  assert_eq(0, @azimuth.multiply(0, min_val))
}

// 测试用例2: 数学恒等式验证
///|
test "mathematical_identities" {
  // 验证基本的数学恒等式
  
  // 乘法对加法的分配律: a * (b + c) = a * b + a * c
  let test_values = [(2, 3, 5), (-2, 3, -5), (10, -3, 7), (-10, -3, -7)]
  
  for (a, b, c) in test_values {
    let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
    let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
    assert_eq(left_side, right_side)
  }
  
  // 零元素性质
  assert_eq(0, @azimuth.multiply(0, 12345))
  assert_eq(0, @azimuth.multiply(12345, 0))
  assert_eq(12345, @azimuth.add(12345, 0))
  assert_eq(12345, @azimuth.add(0, 12345))
}

// 测试用例3: 字符串处理的边界情况
///|
test "string_edge_cases" {
  // 测试各种字符串边界情况
  
  // 空字符串处理
  assert_eq("Hello, !", @azimuth.greet(""))
  
  // 单字符字符串
  assert_eq("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串测试
  let long_name = "VeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLongName"
  let long_greeting = @azimuth.greet(long_name)
  assert_eq(true, long_greeting.contains(long_name))
  
  // 特殊字符组合
  assert_eq("Hello, 🌙⭐🚀!", @azimuth.greet("🌙⭐🚀"))
  assert_eq("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

// 测试用例4: 错误输入处理测试
///|
test "error_handling_scenarios" {
  // 测试各种可能导致错误的输入组合
  
  // 极端值组合测试
  let extreme_values = [2147483647, -2147483648, 2147483646, -2147483647]
  
  for a in extreme_values {
    for b in extreme_values {
      // 确保加法不会导致程序崩溃
      let add_result = @azimuth.add(a, b)
      assert_eq(true, add_result >= -2147483648 && add_result <= 2147483647)
      
      // 确保乘法不会导致程序崩溃
      let mul_result = @azimuth.multiply(a, b)
      assert_eq(true, mul_result >= -2147483648 && mul_result <= 2147483647)
    }
  }
}

// 测试用例5: 实际应用场景 - 购物计算
///|
test "real_world_shopping_calculation" {
  // 模拟真实购物场景的计算
  
  // 商品价格和数量
  let item_prices = [199, 299, 399, 499]
  let item_quantities = [2, 1, 3, 1]
  
  // 计算每种商品的小计
  let subtotal1 = @azimuth.multiply(item_prices[0], item_quantities[0])
  let subtotal2 = @azimuth.multiply(item_prices[1], item_quantities[1])
  let subtotal3 = @azimuth.multiply(item_prices[2], item_quantities[2])
  let subtotal4 = @azimuth.multiply(item_prices[3], item_quantities[3])
  
  // 计算总价
  let total = @azimuth.add(
    @azimuth.add(subtotal1, subtotal2),
    @azimuth.add(subtotal3, subtotal4)
  )
  
  // 验证计算结果: 199*2 + 299*1 + 399*3 + 499*1 = 398 + 299 + 1197 + 499 = 2393
  assert_eq(2393, total)
}

// 测试用例6: 实际应用场景 - 时间计算
///|
test "real_world_time_calculation" {
  // 模拟时间计算场景
  
  // 计算总秒数
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为秒
  let total_seconds = @azimuth.add(
    @azimuth.add(
      @azimuth.multiply(hours, 3600),  // 2小时 = 7200秒
      @azimuth.multiply(minutes, 60)   // 30分钟 = 1800秒
    ),
    seconds                             // 45秒
  )
  
  // 验证结果: 7200 + 1800 + 45 = 9045
  assert_eq(9045, total_seconds)
}

// 测试用例7: 数学序列计算
///|
test "mathematical_sequence_calculation" {
  // 计算斐波那契数列的前几项（使用加法）
  
  // F0 = 0, F1 = 1
  let f0 = 0
  let f1 = 1
  
  // F2 = F0 + F1 = 1
  let f2 = @azimuth.add(f0, f1)
  assert_eq(1, f2)
  
  // F3 = F1 + F2 = 2
  let f3 = @azimuth.add(f1, f2)
  assert_eq(2, f3)
  
  // F4 = F2 + F3 = 3
  let f4 = @azimuth.add(f2, f3)
  assert_eq(3, f4)
  
  // F5 = F3 + F4 = 5
  let f5 = @azimuth.add(f3, f4)
  assert_eq(5, f5)
}

// 测试用例8: 性能压力测试
///|
test "performance_stress_test" {
  // 简单的性能测试 - 连续运算
  
  let base = 1000
  let result = ref base
  
  // 连续加法运算
  for i = 1; i <= 100; i = i + 1 {
    result = @azimuth.add(result, 1)
  }
  
  // 验证结果: 1000 + 100 = 1100
  assert_eq(1100, result)
  
  // 连续乘法运算（小数值避免溢出）
  let mul_result = ref 1
  for i = 1; i <= 5; i = i + 1 {
    mul_result = @azimuth.multiply(mul_result, 2)
  }
  
  // 验证结果: 2^5 = 32
  assert_eq(32, mul_result)
}

// 测试用例9: 边界值组合的乘法测试
///|
test "boundary_multiplication_combinations" {
  // 测试各种边界值组合的乘法
  
  let boundary_values = [0, 1, -1, 2147483647, -2147483648]
  
  for a in boundary_values {
    for b in boundary_values {
      let result = @azimuth.multiply(a, b)
      
      // 验证结果在有效范围内
      assert_eq(true, result >= -2147483648 && result <= 2147483647)
      
      // 验证特定的数学性质
      if a == 0 || b == 0 {
        assert_eq(0, result)
      } else if a == 1 {
        assert_eq(b, result)
      } else if b == 1 {
        assert_eq(a, result)
      } else if a == -1 {
        assert_eq(-b, result)
      } else if b == -1 {
        assert_eq(-a, result)
      }
    }
  }
}

// 测试用例10: 字符串拼接的实际应用
///|
test "practical_string_concatenation" {
  // 测试实际应用中的字符串拼接
  
  // 用户信息
  let user_role = "Developer"
  let user_name = "Alice"
  
  // 创建完整的问候信息
  let role_greeting = @azimuth.greet(user_role)
  let name_greeting = @azimuth.greet(user_name)
  
  // 验证问候信息包含原始输入
  assert_eq(true, role_greeting.contains(user_role))
  assert_eq(true, name_greeting.contains(user_name))
  
  // 验证格式正确
  assert_eq(true, role_greeting.starts_with("Hello, "))
  assert_eq(true, role_greeting.ends_with("!"))
  assert_eq(true, name_greeting.starts_with("Hello, "))
  assert_eq(true, name_greeting.ends_with("!"))
}