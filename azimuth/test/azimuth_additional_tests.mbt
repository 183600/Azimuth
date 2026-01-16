// Azimuth 项目附加测试用例
// 使用标准 MoonBit 测试语法
// 补充测试场景和边界情况

test "zero_value_operations" {
  // 测试零值运算
  
  // 加法零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  
  // 乘法零值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  
  // 负数加法
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
  @azimuth.assert_eq(-5, @azimuth.add(15, -20))
  
  // 负数乘法
  @azimuth.assert_eq(24, @azimuth.multiply(-4, -6))
  @azimuth.assert_eq(-24, @azimuth.multiply(-8, 3))
  @azimuth.assert_eq(-63, @azimuth.multiply(7, -9))
}

test "large_number_operations" {
  // 测试大数运算
  
  // 大数加法
  @azimuth.assert_eq(1000000, @azimuth.add(999999, 1))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(999999, @azimuth.add(1000000, -1))
  
  // 大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(2500, 400))
  @azimuth.assert_eq(1000000, @azimuth.multiply(100000, 10))
}

test "string_boundary_cases" {
  // 测试字符串边界情况
  
  // 长字符串测试
  let long_name = "VeryLongUserNameThatExceedsNormalExpectations"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, a\nb!", @azimuth.greet("a\nb"))
  
  // 混合字符测试
  @azimuth.assert_eq_string("Hello, Test123!@#!", @azimuth.greet("Test123!@#"))
}

test "compound_calculations" {
  // 测试复合计算
  
  // 连续加法
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4)
  @azimuth.assert_eq(10, sum)
  
  // 连续乘法
  let product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 3), 4), 5)
  @azimuth.assert_eq(120, product)
  
  // 混合运算：(a + b) * (c + d)
  let a = 3
  let b = 2
  let c = 4
  let d = 1
  let result = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(c, d))
  @azimuth.assert_eq(25, result)
}

test "mathematical_identities" {
  // 测试数学恒等式
  
  // 加法单位元
  let x = 42
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法单位元
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 加法逆元
  @azimuth.assert_eq(0, @azimuth.add(x, -x))
  @azimuth.assert_eq(0, @azimuth.add(-x, x))
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  
  // 购物计算：商品价格 × 数量 + 税费
  let item_price = 25
  let quantity = 4
  let subtotal = @azimuth.multiply(item_price, quantity)
  let tax = 10
  let total = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(110, total)
  
  // 面积计算：矩形面积 + 正方形面积
  let rect_length = 6
  let rect_width = 4
  let rect_area = @azimuth.multiply(rect_length, rect_width)
  
  let square_side = 5
  let square_area = @azimuth.multiply(square_side, square_side)
  
  let total_area = @azimuth.add(rect_area, square_area)
  @azimuth.assert_eq(49, total_area)
}

test "edge_case_combinations" {
  // 测试边界情况组合
  
  // 零与负数组合
  @azimuth.assert_eq(-5, @azimuth.add(0, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  
  // 一的特殊情况
  @azimuth.assert_eq(-100, @azimuth.multiply(1, -100))
  @azimuth.assert_eq(-100, @azimuth.multiply(-100, 1))
  @azimuth.assert_eq(0, @azimuth.add(1, -1))
}

test "sequential_operations" {
  // 测试序列运算
  
  // 累加计算：1+2+3+4+5
  let acc1 = @azimuth.add(1, 2)
  let acc2 = @azimuth.add(acc1, 3)
  let acc3 = @azimuth.add(acc2, 4)
  let acc4 = @azimuth.add(acc3, 5)
  @azimuth.assert_eq(15, acc4)
  
  // 累乘计算：1*2*3*4
  let mul1 = @azimuth.multiply(1, 2)
  let mul2 = @azimuth.multiply(mul1, 3)
  let mul3 = @azimuth.multiply(mul2, 4)
  @azimuth.assert_eq(24, mul3)
}

test "greeting_function_comprehensive" {
  // 测试问候函数的综合情况
  
  // 数字用户名
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  
  // 空格用户名
  @azimuth.assert_eq_string("Hello,    !", @azimuth.greet("   "))
  
  // 单字符用户名
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 中!", @azimuth.greet("中"))
  
  // 包含空格的用户名
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, 张 三!", @azimuth.greet("张 三"))
}