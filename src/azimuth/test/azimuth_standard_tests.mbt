// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，补充现有测试覆盖范围

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(3000000, @azimuth.add(1000000, 2000000))
  @azimuth.assert_eq(1000000, @azimuth.add(999999, 1))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "add_consecutive_numbers" {
  // 测试连续数字相加
  @azimuth.assert_eq(3, @azimuth.add(1, 2))
  @azimuth.assert_eq(6, @azimuth.add(@azimuth.add(1, 2), 3))
  @azimuth.assert_eq(10, @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(56088, @azimuth.multiply(123, 456))
  @azimuth.assert_eq(998001, @azimuth.multiply(999, 999))
}

test "multiply_by_negative_one" {
  // 测试乘以-1的数学性质
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(17, @azimuth.multiply(-17, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))
}

test "greet_empty_and_whitespace" {
  // 测试空字符串和空白字符的问候
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
}

test "greet_numeric_strings" {
  // 测试数字字符串的问候
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 456789!", @azimuth.greet("456789"))
  @azimuth.assert_eq_string("Hello, 0!", @azimuth.greet("0"))
}

test "mathematical_commutative_property" {
  // 测试加法和乘法的交换律
  // 加法交换律: a + b = b + a
  let result1 = @azimuth.add(15, 27)
  let result2 = @azimuth.add(27, 15)
  @azimuth.assert_eq(result1, result2)
  
  let result3 = @azimuth.add(-5, 10)
  let result4 = @azimuth.add(10, -5)
  @azimuth.assert_eq(result3, result4)
  
  // 乘法交换律: a * b = b * a
  let result5 = @azimuth.multiply(6, 7)
  let result6 = @azimuth.multiply(7, 6)
  @azimuth.assert_eq(result5, result6)
  
  let result7 = @azimuth.multiply(-3, 8)
  let result8 = @azimuth.multiply(8, -3)
  @azimuth.assert_eq(result7, result8)
}

test "mathematical_associative_property" {
  // 测试加法的结合律: (a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  let left = @azimuth.add(@azimuth.add(a, b), c)
  let right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left, right)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let x = 2
  let y = 3
  let z = 4
  let mult_left = @azimuth.multiply(@azimuth.multiply(x, y), z)
  let mult_right = @azimuth.multiply(x, @azimuth.multiply(y, z))
  @azimuth.assert_eq(mult_left, mult_right)
}

test "distributive_property" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "practical_calculations" {
  // 测试实际计算场景
  // 购物计算: 商品价格 * 数量 + 税费
  let item_price = 25
  let quantity = 4
  let tax = 10
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(110, total)
  
  // 平均数计算: (a + b + c) / 3
  let scores_sum = @azimuth.add(@azimuth.add(85, 90), 95)
  // 注意：这里假设没有除法函数，所以只测试求和部分
  @azimuth.assert_eq(270, scores_sum)
}