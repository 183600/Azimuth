// 专注于核心功能的综合测试用例
// 测试 azimuth 包的关键功能和边界情况

test "add_extreme_boundary_values" {
  // 测试加法的极端边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近最大值的加法
  assert_eq(max_val, add(max_val - 10, 10))
  assert_eq(max_val, add(max_val - 100, 100))
  assert_eq(max_val, add(max_val - 1000, 1000))
  
  // 接近最小值的加法
  assert_eq(min_val, add(min_val + 10, -10))
  assert_eq(min_val, add(min_val + 100, -100))
  
  // 混合符号的边界测试
  assert_eq(max_val - 1, add(max_val, -1))
  assert_eq(min_val + 1, add(min_val, 1))
}

test "multiply_precision_boundary" {
  // 测试乘法的精度边界
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘的边界情况
  assert_eq(max_val, multiply(46340, 46341)) // sqrt(max_val) * (sqrt(max_val) + 1)
  assert_eq(max_val, multiply(65536, 32768)) // 2^16 * 2^15
  
  // 负数乘法边界
  assert_eq(min_val, multiply(-46340, 46341))
  assert_eq(min_val, multiply(46340, -46341))
  
  // 最小值相关测试
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -2))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_special_characters" {
  // 测试 greet 函数的特殊字符处理
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \"\'!", greet("\"\'"))
  assert_eq_string("Hello, \u{1F600}!", greet("\u{1F600}")) // emoji
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, 🚀🌙!", greet("🚀🌙"))
  assert_eq_string("Hello, null\\0char!", greet("null\\0char"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值测试
  assert_eq(max_val, divide_with_ceil(max_val, 1))
  assert_eq(min_val, divide_with_ceil(min_val, 1))
  assert_eq(-max_val, divide_with_ceil(-max_val, 1))
  
  // 大数除法
  assert_eq(1073741824, divide_with_ceil(max_val, 2))
  assert_eq(715827883, divide_with_ceil(max_val, 3))
  
  // 负数除法的特殊处理
  assert_eq(-1073741824, divide_with_ceil(min_val, 2))
  assert_eq(-715827882, divide_with_ceil(min_val, 3))
  
  // 小数值测试
  assert_eq(1, divide_with_ceil(1, max_val))
  assert_eq(-1, divide_with_ceil(-1, max_val))
}

test "complex_mathematical_expressions" {
  // 测试复杂的数学表达式
  let a = 100
  let b = 25
  let c = 4
  
  // 多层嵌套运算
  assert_eq(500, multiply(add(a, b), c))
  assert_eq(125, add(multiply(a, b), divide_with_ceil(c, 2)))
  assert_eq(30, divide_with_ceil(add(multiply(a, b), c), 85))
  
  // 负数混合运算
  assert_eq(-300, multiply(add(-a, b), c))
  assert_eq(-75, add(multiply(-a, b), divide_with_ceil(c, 2)))
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续溢出操作
  let result1 = add(max_val, 1) // 应该返回 max_val
  let result2 = add(result1, 1) // 应该仍然返回 max_val
  assert_eq(max_val, result2)
  
  // 连续乘法溢出
  let result3 = multiply(max_val, 2) // 应该返回 max_val
  let result4 = multiply(result3, 2) // 应该仍然返回 max_val
  assert_eq(max_val, result4)
  
  // 最小值的连续操作
  let result5 = add(min_val, 0) // 应该返回 min_val
  let result6 = multiply(result5, 1) // 应该仍然返回 min_val
  assert_eq(min_val, result6)
}

test "performance_critical_operations" {
  // 测试性能关键操作的正确性
  // 这些测试确保常用操作的正确性
  
  // 快速路径测试（零值、单位值）
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
  
  // 负一快速路径
  assert_eq(-42, multiply(-1, 42))
  assert_eq(-42, multiply(42, -1))
  assert_eq(42, multiply(-1, -42))
  
  // 除法的特殊情况
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(4, divide_with_ceil(8, 2))
  assert_eq(3, divide_with_ceil(6, 2))
}

test "unicode_string_comprehensive" {
  // 全面的 Unicode 字符串测试
  assert_eq_string("Hello, 👋!", greet("👋"))
  assert_eq_string("Hello, 🎉🎊🎈!", greet("🎉🎊🎈"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε")) // 希腊字母
  assert_eq_string("Hello, あいうえお!", greet("あいうえお")) // 日文平假名
  assert_eq_string("Hello, العربية!", greet("العربية")) // 阿拉伯文
  assert_eq_string("Hello, עברית!", greet("עברית")) // 希伯来文
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏")) // 地球emoji
}

test "mathematical_invariants" {
  // 测试数学不变量
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法不变量（在安全范围内）
  for i = 0; i < 100; i = i + 1 {
    let x = i - 50 // -50 到 49
    let y = 25
    assert_eq(add(x, y), add(y, x)) // 交换律
    assert_eq(add(add(x, y), 10), add(x, add(y, 10))) // 结合律
  }
  
  // 乘法不变量（在安全范围内）
  for i = 1; i < 10; i = i + 1 {
    let x = i
    let y = 3
    assert_eq(multiply(x, y), multiply(y, x)) // 交换律
    assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2))) // 结合律
  }
  
  // 分配律（在安全范围内）
  for i = 0; i < 10; i = i + 1 {
    let x = i
    let y = 2
    let z = 3
    assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
  }
}