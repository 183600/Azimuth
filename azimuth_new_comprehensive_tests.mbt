// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法

// 测试用例1: 递归计算模拟 - 斐波那契数列
test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列前几项: F(0)=0, F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5
  // 使用展开方式计算，避免递归
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
}

// 测试用例2: 条件逻辑分支测试
test "conditional_logic_branches" {
  // 测试基于不同条件的计算路径
  let x = 10
  let y = 20
  
  // 条件1: x < y
  let result1 = if x < y {
    @azimuth.add(x, y)  // 30
  } else {
    @azimuth.multiply(x, y)
  }
  @azimuth.assert_eq(30, result1)
  
  // 条件2: x > y (false)
  let result2 = if x > y {
    @azimuth.add(x, y)
  } else {
    @azimuth.multiply(x, y)  // 200
  }
  @azimuth.assert_eq(200, result2)
  
  // 条件3: 嵌套条件
  let result3 = if x > 5 {
    if y > 15 {
      @azimuth.multiply(@azimuth.add(x, y), 2)  // (10+20)*2 = 60
    } else {
      @azimuth.add(x, y)
    }
  } else {
    @azimuth.multiply(x, y)
  }
  @azimuth.assert_eq(60, result3)
}

// 测试用例3: 字符串处理边界情况
test "string_processing_edge_cases" {
  // 测试各种字符串边界情况
  // 空字符串
  let empty_result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", empty_result)
  
  // 单个字符
  let single_char_result = @azimuth.greet("A")
  @azimuth.assert_eq_string("Hello, A!", single_char_result)
  
  // 包含空格的字符串
  let spaced_result = @azimuth.greet(" John Doe ")
  @azimuth.assert_eq_string("Hello,  John Doe !", spaced_result)
  
  // 包含特殊字符的字符串
  let special_chars_result = @azimuth.greet("!@#$%^&*()")
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", special_chars_result)
  
  // 包含Unicode字符的字符串
  let unicode_result = @azimuth.greet("🌍世界")
  @azimuth.assert_eq_string("Hello, 🌍世界!", unicode_result)
}

// 测试用例4: 数学函数组合应用
test "mathematical_function_combinations" {
  // 复杂数学表达式计算: (a+b)*(c-d) + e/f
  let a = 15
  let b = 10
  let c = 20
  let d = 8
  let e = 100
  let f = 5
  
  // 分步计算
  let sum_ab = @azimuth.add(a, b)  // 25
  let diff_cd = @azimuth.add(c, -d)  // 12
  let product = @azimuth.multiply(sum_ab, diff_cd)  // 300
  let division = e / f  // 20
  let result = @azimuth.add(product, division)  // 320
  
  @azimuth.assert_eq(320, result)
  
  // 另一个复杂表达式: a*b + c*d - e/f
  let product1 = @azimuth.multiply(a, b)  // 150
  let product2 = @azimuth.multiply(c, d)  // 160
  let sum_products = @azimuth.add(product1, product2)  // 310
  let result2 = @azimuth.add(sum_products, -division)  // 290
  
  @azimuth.assert_eq(290, result2)
}

// 测试用例5: 边界值组合测试
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  let sum_max_min = @azimuth.add(max_val, min_val)  // -1
  @azimuth.assert_eq(-1, sum_max_min)
  
  // 最大值与自身相加（应该溢出）
  let sum_max_max = @azimuth.add(max_val, max_val)  // 应该返回max_val
  @azimuth.assert_eq(max_val, sum_max_max)
  
  // 最小值与自身相加（应该溢出）
  let sum_min_min = @azimuth.add(min_val, min_val)  // 应该返回min_val
  @azimuth.assert_eq(min_val, sum_min_min)
  
  // 最大值与1相乘
  let mul_max_one = @azimuth.multiply(max_val, 1)
  @azimuth.assert_eq(max_val, mul_max_one)
  
  // 最小值与1相乘
  let mul_min_one = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_val, mul_min_one)
  
  // 最小值与-1相乘（特殊情况）
  let mul_min_neg_one = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, mul_min_neg_one)
}

// 测试用例6: 实际应用场景 - 购物车计算
test "shopping_cart_practical_calculation" {
  // 商品价格和数量
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 折扣规则: 满500减50
  let discount_threshold = 500
  let discount_amount = 50
  
  // 运费: 满300免运费，否则15元
  let shipping_threshold = 300
  let shipping_fee = 15
  
  // 计算各商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 598
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 597
  let total3 = @azimuth.multiply(item3_price, item3_quantity)  // 99
  
  // 计算购物车总价
  let subtotal = @azimuth.add(@azimuth.add(total1, total2), total3)  // 1294
  
  // 应用折扣
  let final_discount = if subtotal > discount_threshold {
    discount_amount
  } else {
    0
  }
  let after_discount = @azimuth.add(subtotal, -final_discount)  // 1244
  
  // 计算运费
  let final_shipping = if after_discount > shipping_threshold {
    0
  } else {
    shipping_fee
  }
  
  // 计算最终总价
  let final_total = @azimuth.add(after_discount, final_shipping)  // 1244
  
  @azimuth.assert_eq(1244, final_total)
}

// 测试用例7: 数值范围验证
test "numeric_range_validation" {
  // 测试正数范围
  let small_positive = 1
  let medium_positive = 1000
  let large_positive = 1000000
  
  @azimuth.assert_true(@azimuth.add(small_positive, medium_positive) > 0)
  @azimuth.assert_true(@azimuth.multiply(medium_positive, large_positive) > 0)
  
  // 测试负数范围
  let small_negative = -1
  let medium_negative = -1000
  let large_negative = -1000000
  
  @azimuth.assert_true(@azimuth.add(small_negative, medium_negative) < 0)
  @azimuth.assert_true(@azimuth.multiply(medium_negative, large_negative) > 0)  // 负负得正
  
  // 测试混合符号
  @azimuth.assert_true(@azimuth.add(small_positive, medium_negative) < 0)
  @azimuth.assert_true(@azimuth.multiply(small_positive, large_negative) < 0)
  
  // 测试零值
  @azimuth.assert_eq(0, @azimuth.multiply(0, large_positive))
  @azimuth.assert_eq(large_positive, @azimuth.add(0, large_positive))
  @azimuth.assert_eq(0, @azimuth.add(large_positive, -large_positive))
}

// 测试用例8: 字符串格式化场景
test "string_formatting_scenarios" {
  // 测试不同类型的字符串格式化
  // 数字字符串
  let number_str = "12345"
  let number_greeting = @azimuth.greet(number_str)
  @azimuth.assert_eq_string("Hello, 12345!", number_greeting)
  
  // 邮箱格式
  let email_str = "user@example.com"
  let email_greeting = @azimuth.greet(email_str)
  @azimuth.assert_eq_string("Hello, user@example.com!", email_greeting)
  
  // 电话号码格式
  let phone_str = "+86-138-0013-8000"
  let phone_greeting = @azimuth.greet(phone_str)
  @azimuth.assert_eq_string("Hello, +86-138-0013-8000!", phone_greeting)
  
  // ID格式
  let id_str = "USER-2023-001"
  let id_greeting = @azimuth.greet(id_str)
  @azimuth.assert_eq_string("Hello, USER-2023-001!", id_greeting)
  
  // 版本号格式
  let version_str = "v1.2.3-beta"
  let version_greeting = @azimuth.greet(version_str)
  @azimuth.assert_eq_string("Hello, v1.2.3-beta!", version_greeting)
}

// 测试用例9: 性能计算场景
test "performance_calculation_scenarios" {
  // 计算数列和: 1+2+3+...+n
  let n = 100
  
  // 使用公式计算: n*(n+1)/2
  let series_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, series_sum)
  
  // 计算平方和: 1²+2²+3²+...+n²
  // 使用公式: n*(n+1)*(2n+1)/6
  let square_sum = @azimuth.multiply(@azimuth.multiply(n, @azimuth.add(n, 1)), @azimuth.add(@azimuth.multiply(2, n), 1)) / 6
  @azimuth.assert_eq(338350, square_sum)
  
  // 计算立方和: 1³+2³+3³+...+n³
  // 使用公式: (n*(n+1)/2)²
  let sum_to_n = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  let cube_sum = @azimuth.multiply(sum_to_n, sum_to_n)
  @azimuth.assert_eq(25502500, cube_sum)
}

// 测试用例10: 错误处理与异常场景
test "error_handling_and_exception_scenarios" {
  // 测试边界情况的正确处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)  // 正溢出
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)  // 负溢出
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)  // 正溢出
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)  // 特殊情况处理
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试正常运算
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_true(@azimuth.multiply(4, 6) == 24)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
  
  // 测试边界运算
  @azimuth.assert_true(@azimuth.add(max_val, 0) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, 0) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 0) == 0)
  @azimuth.assert_true(@azimuth.multiply(min_val, 0) == 0)
}