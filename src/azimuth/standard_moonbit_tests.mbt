// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

// 测试减法功能
test "my_subtract_basic_functionality" {
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(10, 10))
  assert_eq(-10, subtract(0, 10))
}

// 测试边界值情况
test "my_boundary_values_test" {
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 1))
  assert_eq(0, subtract(5, 5))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, subtract(5, 0))
}

// 测试负数运算
test "my_negative_number_operations" {
  // 加法
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  
  // 乘法
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  
  // 减法
  assert_eq(-2, subtract(-5, -3))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(8, subtract(5, -3))
}

// 测试字符串处理
test "my_string_handling_edge_cases" {
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 特殊字符
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, Test!", greet("Test"))
  
  // 长字符串
  let long_name = "This is a very long name"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, greet(long_name))
}

// 测试除法的各种情况
test "my_divide_with_ceil_comprehensive" {
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 零值处理
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

// 实际应用场景：计算平均分
test "my_average_score_calculation" {
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 95
  let score5 = 88
  let total = add(add(add(add(score1, score2), score3), score4), score5)
  let count = 5
  let average = divide_with_ceil(total, count)
  assert_eq(88, average)  // ceil(438/5) = 88
}

// 实际应用场景：计算折扣价格
test "my_discount_price_calculation" {
  let original_price = 100
  let discount_percent = 20
  let discount_amount = divide_with_ceil(multiply(original_price, discount_percent), 100)
  let final_price = subtract(original_price, discount_amount)
  assert_eq(80, final_price)  // 100 - ceil(100*20/100) = 100 - 20 = 80
}

// 实际应用场景：批量计算
test "my_batch_calculation" {
  let qty1 = 10
  let qty2 = 15
  let qty3 = 20
  let qty4 = 25
  let unit_price = 5
  let total1 = multiply(qty1, unit_price)
  let total2 = multiply(qty2, unit_price)
  let total3 = multiply(qty3, unit_price)
  let total4 = multiply(qty4, unit_price)
  let grand_total = add(add(add(total1, total2), total3), total4)
  assert_eq(350, grand_total)  // (10+15+20+25)*5 = 70*5 = 350
}

// 测试复杂表达式
test "my_complex_expressions" {
  // 嵌套运算
  let result1 = add(multiply(2, add(3, 4)), subtract(10, divide_with_ceil(15, 4)))
  assert_eq(20, result1)  // 2*(3+4) + (10-4) = 14 + 6 = 20
  
  // 多重嵌套
  let result2 = multiply(
    add(divide_with_ceil(20, 3), subtract(10, 5)),
    divide_with_ceil(subtract(15, 3), 4)
  )
  assert_eq(36, result2)  // (7+5)*3 = 12*3 = 36
}