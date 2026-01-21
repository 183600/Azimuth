// Azimuth 增强标准测试用例
// 使用标准的 MoonBit 测试语法，补充测试覆盖

test "add_floating_point_behavior" {
  // 测试加法在接近边界时的行为
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近最大值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 10, 10))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 1000))
  
  // 测试接近最小值的加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 10, -10))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000, -1000))
}

test "multiply_precision_edge_cases" {
  // 测试乘法精度边界情况
  let sqrt_max = 46340  // sqrt(2147483647) 的整数部分
  
  // 测试接近溢出边界的乘法
  @azimuth.assert_eq(2147395600, @azimuth.multiply(sqrt_max, sqrt_max))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(sqrt_max, sqrt_max + 1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(sqrt_max + 1, sqrt_max))
  
  // 测试负数精度
  @azimuth.assert_eq(-2147395600, @azimuth.multiply(sqrt_max, -sqrt_max))
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(sqrt_max, -(sqrt_max + 1)))
}

test "divide_with_ceil_fractional_behavior" {
  // 测试向上取整除法的小数行为
  // 测试余数为1的情况
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  
  // 测试余数为除数减1的情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(14, 3))
  
  // 测试负分数的向上取整
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
}

test "greet_function_edge_case_strings" {
  // 测试问候函数的边界字符串
  // 测试单字符
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, Z!", @azimuth.greet("Z"))
  @azimuth.assert_eq_string("Hello, 9!", @azimuth.greet("9"))
  
  // 测试特殊字符组合
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, \"\'!", @azimuth.greet("\"\'"))
  @azimuth.assert_eq_string("Hello, &%!", @azimuth.greet("&%"))
  
  // 测试中文字符
  @azimuth.assert_eq_string("Hello, 你好!", @azimuth.greet("你好"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(123, 456), @azimuth.add(456, 123))
  @azimuth.assert_eq(@azimuth.add(-100, 200), @azimuth.add(200, -100))
  
  // 加法结合律（在安全范围内）
  let a = 100
  let b = 200
  let c = 300
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(12, 34), @azimuth.multiply(34, 12))
  @azimuth.assert_eq(@azimuth.multiply(-5, 7), @azimuth.multiply(7, -5))
  
  // 乘法结合律（在安全范围内）
  let x = 10
  let y = 20
  let z = 30
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
}

test "complex_nested_operations" {
  // 测试复杂的嵌套运算
  // 计算: ((a + b) * c) + (d / e)
  let a = 15
  let b = 25
  let c = 3
  let d = 20
  let e = 6
  
  let sum_ab = @azimuth.add(a, b)
  let product_abc = @azimuth.multiply(sum_ab, c)
  let quotient_de = @azimuth.divide_with_ceil(d, e)
  let final_result = @azimuth.add(product_abc, quotient_de)
  
  @azimuth.assert_eq(123, final_result)  // ((15+25)*3) + ceil(20/6) = 40*3 + 4 = 124
  
  // 更复杂的嵌套: ceil((a * (b + ceil(c/d))) / e)
  let f = 8
  let g = 3
  let h = 10
  let i = 4
  let j = 7
  let cd_ceil = @azimuth.divide_with_ceil(h, i)
  let b_plus_cd = @azimuth.add(g, cd_ceil)
  let a_times = @azimuth.multiply(f, b_plus_cd)
  let complex_result = @azimuth.divide_with_ceil(a_times, j)
  
  @azimuth.assert_eq(14, complex_result)  // ceil((8 * (3 + ceil(10/4))) / 7) = ceil((8 * 6) / 7) = ceil(48/7) = 7
}

test "boundary_value_stress_test" {
  // 边界值压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续运算不会导致状态错误
  for i = 0; i < 10; i = i + 1 {
    let _ = @azimuth.add(100, 200)
    let _ = @azimuth.multiply(5, 6)
    let _ = @azimuth.divide_with_ceil(15, 4)
  }
  
  // 验证极值运算后函数仍然正常
  @azimuth.assert_eq(300, @azimuth.add(100, 200))
  @azimuth.assert_eq(30, @azimuth.multiply(5, 6))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(15, 4))
  
  // 测试极值运算不会影响正常运算
  let _ = @azimuth.add(max_val, max_val)  // 溢出
  let _ = @azimuth.multiply(max_val, 2)   // 溢出
  @azimuth.assert_eq(8, @azimuth.add(3, 5))  // 正常运算应该仍然正确
}

test "assert_functions_comprehensive" {
  // 断言函数综合测试
  // 测试各种相等性断言
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  @azimuth.assert_eq(123456789, 123456789)
  
  // 测试字符串相等性断言
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("single", "single")
  @azimuth.assert_eq_string("Long string with spaces and symbols!@#$%", "Long string with spaces and symbols!@#$%")
  @azimuth.assert_eq_string("Unicode: αβγδεζηθ", "Unicode: αβγδεζηθ")
  
  // 测试布尔断言的复杂条件
  @azimuth.assert_true(1 + 1 == 2)
  @azimuth.assert_true(100 - 50 == 50)
  @azimuth.assert_true(-5 + 10 == 5)
  @azimuth.assert_true(0 * 12345 == 0)
  
  @azimuth.assert_false(1 + 1 == 3)
  @azimuth.assert_false(100 - 50 == 51)
  @azimuth.assert_false(-5 + 10 == 6)
  @azimuth.assert_false(0 * 12345 == 1)
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  // 场景1：计算购物车总价（含税）
  let item1 = 299
  let item2 = 399
  let item3 = 199
  let tax_rate = 3  // 1/3 税率
  
  let subtotal = @azimuth.add(@azimuth.add(item1, item2), item3)
  let total_with_tax = @azimuth.multiply(subtotal, tax_rate)
  @azimuth.assert_eq(2691, total_with_tax)  // (299+399+199)*3 = 897*3 = 2691
  
  // 场景2：计算平均分（向上取整）
  let score1 = 87
  let score2 = 93
  let score3 = 79
  let score4 = 85
  
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4)
  let average_score = @azimuth.divide_with_ceil(total_score, 4)
  @azimuth.assert_eq(87, average_score)  // ceil((87+93+79+85)/4) = ceil(344/4) = 86
  
  // 场景3：计算页面布局（网格系统）
  let container_width = 1200
  let columns = 5
  let gutter_width = 20
  let total_gutter = @azimuth.multiply(gutter_width, @azimuth.subtract(columns, 1))
  let column_width = @azimuth.divide_with_ceil(@azimuth.subtract(container_width, total_gutter), columns)
  @azimuth.assert_eq(224, column_width)  // ceil((1200 - 20*4)/5) = ceil(1120/5) = 224
}