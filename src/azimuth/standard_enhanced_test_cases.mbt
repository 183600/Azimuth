// 标准增强测试用例 - 使用标准 MoonBit 测试语法
// 覆盖核心功能和边界情况

test "large_number_addition" {
  // 测试大数加法运算
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000000, -1500000000))
}

test "negative_multiplication" {
  // 测试负数乘法运算
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-100, 0))
}

test "special_character_string_processing" {
  // 测试包含特殊字符的字符串处理
  @azimuth.assert_eq_string("Hello, John@Doe!", @azimuth.greet("John@Doe"))
  @azimuth.assert_eq_string("Hello, 用户#123!", @azimuth.greet("用户#123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "boundary_value_combinations" {
  // 测试边界值组合运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_associativity" {
  // 测试数学运算的结合律
  // 加法结合律: (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let x = 5
  let y = 10
  let z = 20
  
  let result3 = @azimuth.multiply(@azimuth.multiply(x, y), z)
  let result4 = @azimuth.multiply(x, @azimuth.multiply(y, z))
  @azimuth.assert_eq(result3, result4)
}

test "shopping_cart_calculation" {
  // 测试实际应用场景：购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(total1, total2)
  
  // 验证总价：299*2 + 199*3 = 598 + 597 = 1195
  @azimuth.assert_eq(1195, cart_total)
}

test "error_handling_and_boundaries" {
  // 测试错误处理和边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "compound_operations" {
  // 测试复合运算（多个函数组合使用）
  // 计算矩形周长：2 * (长 + 宽)
  let length = 15
  let width = 10
  
  let sum = @azimuth.add(length, width)
  let perimeter = @azimuth.multiply(2, sum)
  
  @azimuth.assert_eq(50, perimeter)
  
  // 计算平均数：(a + b + c) / 3
  let a = 90
  let b = 80
  let c = 70
  
  let total = @azimuth.add(@azimuth.add(a, b), c)
  let average = total / 3
  
  @azimuth.assert_eq(80, average)
}

test "string_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  let long_name = "VeryLongNameThatTestsStringLengthHandling"
  @azimuth.assert_eq_string("Hello, VeryLongNameThatTestsStringLengthHandling!", @azimuth.greet(long_name))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "mathematical_commutativity" {
  // 测试数学运算的交换律
  // 加法交换律: a + b = b + a
  let a = 123
  let b = 456
  
  let add_result1 = @azimuth.add(a, b)
  let add_result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(add_result1, add_result2)
  
  // 乘法交换律: a * b = b * a
  let x = 12
  let y = 34
  
  let mul_result1 = @azimuth.multiply(x, y)
  let mul_result2 = @azimuth.multiply(y, x)
  @azimuth.assert_eq(mul_result1, mul_result2)
  
  // 测试包含负数的情况
  let m = -15
  let n = 25
  
  let neg_add1 = @azimuth.add(m, n)
  let neg_add2 = @azimuth.add(n, m)
  @azimuth.assert_eq(neg_add1, neg_add2)
  
  let neg_mul1 = @azimuth.multiply(m, n)
  let neg_mul2 = @azimuth.multiply(n, m)
  @azimuth.assert_eq(neg_mul1, neg_mul2)
}