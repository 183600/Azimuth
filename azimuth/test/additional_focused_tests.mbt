// 额外的聚焦测试用例 - 针对特定场景的测试

test "add_function_symmetry_property" {
  // 测试加法的对称性
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-15, 27), @azimuth.add(27, -15))
  @azimuth.assert_eq(@azimuth.add(-15, -27), @azimuth.add(-27, -15))
}

test "multiply_function_associative_property" {
  // 测试乘法的结合性
  let a = 4
  let b = 5
  let c = 6
  
  // (a * b) * c = a * (b * c)
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "greet_function_whitespace_handling" {
  // 测试问候函数对空白字符的处理
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \t\t!", @azimuth.greet("\t\t"))
  @azimuth.assert_eq_string("Hello, \n\n!", @azimuth.greet("\n\n"))
  @azimuth.assert_eq_string("Hello,  \t\n!", @azimuth.greet(" \t\n"))
}

test "divide_with_ceil_practical_scenarios" {
  // 测试向上取整除法的实际应用场景
  
  // 场景1：计算需要的车辆数量（每辆车最多坐4人）
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(11, 4))  // 11人需要3辆车
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))   // 8人需要2辆车
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 4))   // 3人需要1辆车
  
  // 场景2：计算需要的页面数量（每页最多显示10个项目）
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(42, 10))  // 42个项目需要5页
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(40, 10))  // 40个项目需要4页
}

test "complex_calculation_real_world" {
  // 测试复杂计算的实际应用
  
  // 计算总价：商品价格 × 数量 + 运费
  let unit_price = 25
  let quantity = 7
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(190, total)  // 25*7 + 15 = 175 + 15 = 190
}

test "edge_case_single_character_strings" {
  // 测试单字符字符串的边界情况
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, Z!", @azimuth.greet("Z"))
  @azimuth.assert_eq_string("Hello, 5!", @azimuth.greet("5"))
  @azimuth.assert_eq_string("Hello, @!", @azimuth.greet("@"))
  @azimuth.assert_eq_string("Hello, 中!", @azimuth.greet("中"))
}

test "number_sequence_calculations" {
  // 测试数列计算
  
  // 等差数列求和：1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let sum4 = @azimuth.add(sum3, 5)
  @azimuth.assert_eq(15, sum4)
  
  // 等比数列求积：1 × 2 × 3 × 4
  let product1 = @azimuth.multiply(1, 2)
  let product2 = @azimuth.multiply(product1, 3)
  let product3 = @azimuth.multiply(product2, 4)
  @azimuth.assert_eq(24, product3)
}

test "mixed_operations_with_negative_numbers" {
  // 测试负数的混合运算
  let a = -10
  let b = 5
  let c = -3
  
  // (-10 + 5) * (-3)
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(15, result)  // (-5) * (-3) = 15
  
  // (-10) * 5 + (-3)
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(-53, result2)  // (-50) + (-3) = -53
}

test "divide_with_ceil_boundary_precision" {
  // 测试向上取整除法的边界精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(2, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, -2))
}