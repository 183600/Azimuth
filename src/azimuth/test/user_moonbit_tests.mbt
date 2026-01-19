// 用户自定义的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 测试连续加法运算
test "test_sequential_addition" {
  // 测试 (a + b) + (c + d) 的计算
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  // 测试包含负数的连续加法
  let result2 = @azimuth.add(@azimuth.add(-5, 10), @azimuth.add(-3, 8))
  @azimuth.assert_eq(10, result2)
}

// 测试连续乘法运算
test "test_sequential_multiplication" {
  // 测试 (a * b) * (c * d) 的计算
  let result1 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result1)
  
  // 测试包含负数的连续乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(120, result2)
}

// 测试极值运算组合
test "test_extreme_value_operations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试极值与接近极值的数相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
}

// 测试字符串处理边界情况
test "test_string_edge_cases" {
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystems"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result)
  
  // 测试包含特殊字符的字符串
  let special_name = "John@Doe#123$%^&*()"
  let result2 = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, John@Doe#123$%^&*()!", result2)
}

// 测试加法结合律
test "test_addition_associativity" {
  let a = 100
  let b = 200
  let c = 300
  
  // 验证 (a + b) + c = a + (b + c)
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let d = -150
  let e = 250
  let f = -100
  
  let result3 = @azimuth.add(@azimuth.add(d, e), f)
  let result4 = @azimuth.add(d, @azimuth.add(e, f))
  @azimuth.assert_eq(result3, result4)
}

// 测试乘法交换律
test "test_multiplication_commutativity" {
  let a = 12
  let b = 15
  
  // 验证 a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试包含负数的情况
  let c = -20
  let d = 30
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

// 测试实际应用场景：订单计算
test "test_order_calculation" {
  let item_price = 299
  let quantity = 3
  let shipping = 15
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  // 计算最终总价（含运费）
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(1081, total)  // (299*3) + (299*3*8/100) + 15 = 897 + 71 + 15 = 983
}

// 测试数学公式验证：分配律
test "test_distributive_property" {
  let a = 7
  let b = 3
  let c = 5
  
  // 验证 a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

// 测试平均值计算
test "test_average_calculation" {
  // 计算三个数的平均值：(a + b + c) / 3
  let num1 = 90
  let num2 = 80
  let num3 = 85
  
  // 计算总和：90 + 80 + 85 = 255
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)
  // 计算平均值：255 / 3 = 85（整数除法）
  let average = sum / 3
  
  @azimuth.assert_eq(85, average)
}

// 测试几何计算：面积和周长
test "test_geometry_calculations" {
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}