// 额外的标准 MoonBit 测试用例
// 测试 azimuth 包中的核心函数的各种场景

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "mathematical_properties_associative" {
  // 测试数学结合律性质
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_add = @azimuth.add(@azimuth.add(a, b), c)
  let right_add = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_add, right_add)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_mul = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mul = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mul, right_mul)
}

test "mathematical_properties_distributive" {
  // 测试数学分配律性质: a * (b + c) = a * b + a * c
  let a = 5
  let b = 12
  let c = 8
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "boundary_value_comprehensive" {
  // 综合测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, neg_one))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, one))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, neg_one))
  
  // 零值测试
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, max_val))
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, min_val))
}

test "string_processing_comprehensive" {
  // 综合测试字符串处理
  // 测试各种字符串类型
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, space separated!", @azimuth.greet("space separated"))
}

test "real_world_calculations" {
  // 实际应用场景计算测试
  // 场景1: 计算矩形周长 = 2 * (长 + 宽)
  let length = 15
  let width = 10
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(50, perimeter)
  
  // 场景2: 计算折扣价格 = 原价 - 折扣金额
  let original_price = 200
  let discount_rate = 20  // 20% 折扣
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(160, discounted_price)
  
  // 场景3: 计算复利（简化版）= 本金 + 本金 * 利率 * 年数
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(16000, total)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 场景1: 计算多项式值 f(x) = 2x² + 3x + 5，当 x = 4
  let x = 4
  let x_squared = @azimuth.multiply(x, x)
  let term1 = @azimuth.multiply(2, x_squared)
  let term2 = @azimuth.multiply(3, x)
  let polynomial_result = @azimuth.add(@azimuth.add(term1, term2), 5)
  @azimuth.assert_eq(53, polynomial_result)
  
  // 场景2: 计算购物车总价
  let item1_price = 99
  let item1_qty = 2
  let item2_price = 149
  let item2_qty = 1
  let item3_price = 79
  let item3_qty = 3
  
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let total3 = @azimuth.multiply(item3_price, item3_qty)
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  @azimuth.assert_eq(524, cart_total)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "performance_related_operations" {
  // 性能相关操作测试
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num1, large_num2))
  
  // 测试连续运算
  let result = @azimuth.add(@azimuth.multiply(10, 20), @azimuth.multiply(30, 40))
  @azimuth.assert_eq(1400, result)
  
  // 测试嵌套运算
  let nested = @azimuth.multiply(@azimuth.add(5, 10), @azimuth.add(15, 20))
  @azimuth.assert_eq(525, nested)
}

test "data_validation_scenarios" {
  // 数据验证场景测试
  // 测试输入验证逻辑
  let positive_num = 100
  let negative_num = -50
  let zero_num = 0
  
  // 验证正数运算
  @azimuth.assert_true(@azimuth.add(positive_num, positive_num) > 0)
  @azimuth.assert_true(@azimuth.multiply(positive_num, positive_num) > 0)
  
  // 验证负数运算
  @azimuth.assert_true(@azimuth.add(negative_num, negative_num) < 0)
  @azimuth.assert_true(@azimuth.multiply(negative_num, negative_num) > 0)
  
  // 验证混合符号运算
  let mixed_add_result = @azimuth.add(positive_num, negative_num)
  @azimuth.assert_eq(50, mixed_add_result)  // 100 + (-50) = 50
  @azimuth.assert_true(@azimuth.multiply(positive_num, negative_num) < 0)
  
  // 验证零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(zero_num, positive_num))
  @azimuth.assert_eq(positive_num, @azimuth.add(zero_num, positive_num))
}