// 增强综合测试用例
// 为 Azimuth 数学库添加高质量测试覆盖
// 使用标准 MoonBit 测试语法

test "add_function_boundary_conditions" {
  // 测试 add 函数的边界条件
  @azimuth.assert_eq(Int.max_value(), @azimuth.add(Int.max_value() - 1, 1))
  @azimuth.assert_eq(Int.min_value(), @azimuth.add(Int.min_value() + 1, -1))
  @azimuth.assert_eq(0, @azimuth.add(Int.max_value(), Int.min_value() + 1))
  @azimuth.assert_eq(1, @azimuth.add(Int.max_value(), Int.min_value() + 2))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界和特殊情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, Int.max_value()))
  @azimuth.assert_eq(0, @azimuth.multiply(0, Int.min_value()))
  @azimuth.assert_eq(Int.max_value(), @azimuth.multiply(1, Int.max_value()))
  @azimuth.assert_eq(Int.min_value(), @azimuth.multiply(1, Int.min_value()))
  @azimuth.assert_eq(Int.max_value(), @azimuth.multiply(-1, Int.min_value()))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试 divide_with_ceil 函数
  // 正数除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 3))
  
  // 负数除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(7, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-7, -3))
  
  // 除零保护
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "greet_function_unicode_handling" {
  // 测试 greet 函数的 Unicode 字符串处理
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🚀🌟!", @azimuth.greet("🚀🌟"))
  @azimuth.assert_eq_string("Hello, space!", @azimuth.greet("space"))
}

test "business_scenario_calculations" {
  // 测试业务场景计算
  // 购物车计算：商品价格 × 数量 + 税费 + 运费
  let unit_price = 29
  let quantity = 3
  let tax_rate = 8
  let shipping = 12
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(87, subtotal)
  @azimuth.assert_eq(7, tax)
  @azimuth.assert_eq(106, total)
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = 17
  
  // 加法单位元
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(0, y))
  
  // 乘法单位元
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(y, @azimuth.multiply(1, y))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, y))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), 5), @azimuth.add(x, @azimuth.add(y, 5)))
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值
  // 计算：(a * b) + (c / d) - e，其中除法使用向上取整
  let a = 15
  let b = 4
  let c = 20
  let d = 6
  let e = 8
  
  let product = @azimuth.multiply(a, b)
  let quotient = @azimuth.divide_with_ceil(c, d)
  let sum = @azimuth.add(product, quotient)
  let result = @azimuth.add(sum, -e)
  
  @azimuth.assert_eq(60, product)
  @azimuth.assert_eq(4, quotient)
  @azimuth.assert_eq(64, sum)
  @azimuth.assert_eq(56, result)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  // 大数运算稳定性
  let big_num = 10000
  @azimuth.assert_eq(20000, @azimuth.add(big_num, big_num))
  @azimuth.assert_eq(100000000, @azimuth.multiply(big_num, big_num))
  
  // 混合正负数运算
  @azimuth.assert_eq(0, @azimuth.add(500, -500))
  @azimuth.assert_eq(-250000, @azimuth.multiply(500, -500))
  @azimuth.assert_eq(-166, @azimuth.divide_with_ceil(-500, 3))
}