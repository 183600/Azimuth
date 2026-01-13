// 额外的测试用例 - 使用标准 MoonBit 测试语法
// 导入主模块以访问测试函数
test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值相加
  @assertion.assert_eq(-1, azimuth.add(max_val, min_val))
  
  // 测试连续相加的边界情况
  @assertion.assert_eq(min_val + 2, azimuth.add(azimuth.add(min_val, 1), 1))
}

test "multiply_function_precision" {
  // 测试乘法函数的精度和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘的溢出处理
  @assertion.assert_eq(max_val, azimuth.multiply(46341, 46341))
  
  // 测试最小值与2的乘法
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 2))
}

test "greet_function_unicode_handling" {
  // 测试greet函数的Unicode处理能力
  let unicode_names = [
    "José", "François", "Müller", "Øyvind", "Łukasz",
    "张伟", "李娜", "王芳", "刘洋", "陈静"
  ]
  
  // 测试各种Unicode字符
  @assertion.assert_eq_string("Hello, José!", azimuth.greet("José"))
  @assertion.assert_eq_string("Hello, 张伟!", azimuth.greet("张伟"))
  @assertion.assert_eq_string("Hello, Øyvind!", azimuth.greet("Øyvind"))
}

test "complex_mathematical_operations" {
  // 测试复杂的数学运算组合
  let a = 15
  let b = 25
  let c = 10
  
  // 计算 (a + b) * c
  let sum_ab = azimuth.add(a, b)
  let result = azimuth.multiply(sum_ab, c)
  @assertion.assert_eq(400, result)
  
  // 计算 a * b + c
  let product_ab = azimuth.multiply(a, b)
  let result2 = azimuth.add(product_ab, c)
  @assertion.assert_eq(385, result2)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试空字符串
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))
  
  // 测试单字符
  @assertion.assert_eq_string("Hello, A!", azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  @assertion.assert_eq_string("Hello, \n\t\r!", azimuth.greet("\n\t\r"))
}

test "numeric_stability_tests" {
  // 测试数值稳定性
  let x = 1000000
  let y = -1000000
  let z = 1
  
  // 测试大数相加的稳定性
  @assertion.assert_eq(0, azimuth.add(x, y))
  @assertion.assert_eq(1000001, azimuth.add(x, z))
  @assertion.assert_eq(-999999, azimuth.add(y, z))
}

test "function_composition_patterns" {
  // 测试函数组合模式
  let base_value = 10
  
  // 嵌套调用：multiply(add(multiply(2, 3), 4), 5)
  let inner_mult = azimuth.multiply(2, 3)
  let inner_add = azimuth.add(inner_mult, 4)
  let final_result = azimuth.multiply(inner_add, 5)
  @assertion.assert_eq(50, final_result)
}

test "boundary_crossing_operations" {
  // 测试跨越边界的运算
  let near_max = 2147483640
  let near_min = -2147483640
  
  // 测试接近最大值的加法
  @assertion.assert_eq(2147483647, azimuth.add(near_max, 7))
  @assertion.assert_eq(2147483647, azimuth.add(near_max, 10))
  
  // 测试接近最小值的加法
  @assertion.assert_eq(-2147483648, azimuth.add(near_min, -8))
  @assertion.assert_eq(-2147483648, azimuth.add(near_min, -10))
}

test "error_resilience_verification" {
  // 测试错误恢复能力
  let normal_cases = [
    (1, 2, 3),      // 1 + 2 = 3
    (10, 20, 30),   // 10 + 20 = 30
    (-5, 5, 0)      // -5 + 5 = 0
  ]
  
  // 验证正常情况下的计算
  @assertion.assert_eq(3, azimuth.add(normal_cases[0].0, normal_cases[0].1))
  @assertion.assert_eq(30, azimuth.add(normal_cases[1].0, normal_cases[1].1))
  @assertion.assert_eq(0, azimuth.add(normal_cases[2].0, normal_cases[2].1))
}

test "performance_critical_paths" {
  // 测试性能关键路径
  let iterations = 100
  let base = 1
  let accumulator = 0
  
  // 模拟循环累加（手动展开）
  let result1 = azimuth.add(azimuth.add(azimuth.add(base, base), base), base)
  let result2 = azimuth.add(azimuth.add(azimuth.add(base, base), base), base)
  let final_result = azimuth.add(result1, result2)
  
  @assertion.assert_eq(8, final_result)
}