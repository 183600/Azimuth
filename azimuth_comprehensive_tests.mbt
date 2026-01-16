// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，提供全面的功能测试覆盖

test "basic_addition_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(10, -8))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(4, -5))
}

test "mathematical_identity_properties" {
  // 数学恒等性质测试
  
  // 加法单位元: a + 0 = a
  let test_values = [0, 1, -1, 42, -100]
  for value in test_values {
    @azimuth.assert_eq(value, @azimuth.add(value, 0))
    @azimuth.assert_eq(value, @azimuth.add(0, value))
  }
  
  // 乘法单位元: a * 1 = a
  for value in test_values {
    @azimuth.assert_eq(value, @azimuth.multiply(value, 1))
    @azimuth.assert_eq(value, @azimuth.multiply(1, value))
  }
}

test "zero_property_tests" {
  // 零性质测试
  
  // 零加法: a + (-a) = 0
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(0, @azimuth.add(-25, 25))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 零乘法: a * 0 = 0
  let test_values = [1, -1, 42, -100, 1000]
  for value in test_values {
    @azimuth.assert_eq(0, @azimuth.multiply(value, 0))
    @azimuth.assert_eq(0, @azimuth.multiply(0, value))
  }
}

test "calculation_sequence_tests" {
  // 计算序列测试
  
  // 阶乘计算: 5! = 5 * 4 * 3 * 2 * 1
  let factorial_5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_5)
  
  // 幂计算: 2^4 = 2 * 2 * 2 * 2
  let power_2_4 = @azimuth.multiply(2, @azimuth.multiply(2, @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(16, power_2_4)
  
  // 平方和: 3² + 4² = 9 + 16 = 25
  let square_sum = @azimuth.add(@azimuth.multiply(3, 3), @azimuth.multiply(4, 4))
  @azimuth.assert_eq(25, square_sum)
}

test "real_world_application_tests" {
  // 实际应用测试
  
  // 购物车总价: 商品价格 * 数量
  let apple_price = 5
  let apple_quantity = 8
  let orange_price = 3
  let orange_quantity = 12
  
  let apple_total = @azimuth.multiply(apple_price, apple_quantity)
  let orange_total = @azimuth.multiply(orange_price, orange_quantity)
  let cart_total = @azimuth.add(apple_total, orange_total)
  @azimuth.assert_eq(76, cart_total)
  
  // 税收计算: 价格 * 税率
  let price = 100
  let tax_rate = 8 // 8%
  let tax_amount = @azimuth.multiply(price, tax_rate) / 100
  let final_price = @azimuth.add(price, tax_amount)
  @azimuth.assert_eq(108, final_price)
}

test "boundary_value_tests" {
  // 边界值测试
  
  // 小数值测试
  @azimuth.assert_eq(2, @azimuth.add(1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(0, @azimuth.add(1, -1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  
  // 大数值测试
  let large_num = 10000
  @azimuth.assert_eq(20000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(100000000, @azimuth.multiply(large_num, large_num))
  
  // 负边界测试
  let neg_num = -1000
  @azimuth.assert_eq(-2000, @azimuth.add(neg_num, neg_num))
  @azimuth.assert_eq(1000000, @azimuth.multiply(neg_num, neg_num))
}

test "algebraic_property_verification" {
  // 代数性质验证测试
  
  let a = 15
  let b = 25
  let c = 10
  
  // 交换律验证
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 结合律验证
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律验证
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  
  // 连续零运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, @azimuth.multiply(0, 0)))
  @azimuth.assert_eq(0, @azimuth.add(0, @azimuth.add(0, 0)))
  
  // 混合正负运算
  let pos = 100
  let neg = -50
  
  @azimuth.assert_eq(50, @azimuth.add(pos, neg))
  @azimuth.assert_eq(-5000, @azimuth.multiply(pos, neg))
  @azimuth.assert_eq(-150, @azimuth.add(neg, -pos))
  @azimuth.assert_eq(2500, @azimuth.multiply(neg, neg))
  
  // 单调性测试
  let base = 10
  @azimuth.assert_eq(@azimuth.add(base, 5), @azimuth.add(@azimuth.add(base, 3), 2))
  @azimuth.assert_eq(@azimuth.multiply(base, 3), @azimuth.add(@azimuth.multiply(base, 2), base))
}