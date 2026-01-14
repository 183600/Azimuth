// 新的 MoonBit 测试用例 - 覆盖核心功能和边界情况
// 导入 azimuth 包以使用其中的函数

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, azimuth::add(7, 3))
  assert_eq(0, azimuth::add(5, -5))
  assert_eq(-10, azimuth::add(-7, -3))
  
  assert_eq(15, azimuth::multiply(3, 5))
  assert_eq(0, azimuth::multiply(8, 0))
  assert_eq(-12, azimuth::multiply(-3, 4))
}

test "extreme_boundary_values" {
  // 测试极值边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  
  // 最小值运算
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  
  // 极值相加
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "string_processing_variations" {
  // 测试字符串处理的各种变化
  // 数字字符串
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  
  // 特殊符号
  assert_eq_string("Hello, @#$%!", azimuth::greet("@#$%"))
  
  // 混合内容
  assert_eq_string("Hello, User123!", azimuth::greet("User123"))
  
  // 空格处理
  assert_eq_string("Hello,  !", azimuth::greet(" "))
}

test "mathematical_series_calculations" {
  // 测试数学数列计算
  // 等差数列前5项和：2+4+6+8+10 = 30
  let sum1 = azimuth::add(azimuth::add(2, 4), azimuth::add(6, azimuth::add(8, 10)))
  assert_eq(30, sum1)
  
  // 等比数列前4项积：2×4×8×16 = 1024
  let product1 = azimuth::multiply(2, azimuth::multiply(4, azimuth::multiply(8, 16)))
  assert_eq(1024, product1)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 银行利息计算
  let principal = 10000
  let rate = 5
  let years = 3
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, years))
  let total = azimuth::add(principal, interest)
  assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
  
  // 面积计算
  let length = 12
  let width = 8
  let area = azimuth::multiply(length, width)
  assert_eq(96, area)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护验证
  assert_eq(max_val, azimuth::add(max_val, 100))
  assert_eq(min_val, azimuth::add(min_val, -100))
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, -2))
  
  // 特殊值处理
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "function_composition_patterns" {
  // 测试函数组合模式
  // 复合计算：(a+b)×(c-d)
  let a = 8
  let b = 2
  let c = 15
  let d = 5
  
  let sum_ab = azimuth::add(a, b)
  let diff_cd = azimuth::add(c, -d)
  let result = azimuth::multiply(sum_ab, diff_cd)
  
  assert_eq(100, result)  // (8+2)×(15-5) = 10×10 = 100
}

test "conditional_calculations" {
  // 测试条件计算
  let x = 15
  let y = 20
  
  // 条件：如果x<y则相加，否则相乘
  let condition = x < y
  let result = if condition { azimuth::add(x, y) } else { azimuth::multiply(x, y) }
  assert_eq(35, result)  // 15<20为真，所以15+20=35
  
  // 反向条件
  let result2 = if x > y { azimuth::add(x, y) } else { azimuth::multiply(x, y) }
  assert_eq(300, result2)  // 15>20为假，所以15×20=300
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符
  // 各种语言
  assert_eq_string("Hello, Москва!", azimuth::greet("Москва"))
  assert_eq_string("Hello, القاهرة!", azimuth::greet("القاهرة"))
  assert_eq_string("Hello, 東京!", azimuth::greet("東京"))
  
  // 复合Emoji
  assert_eq_string("Hello, 👨‍💻!", azimuth::greet("👨‍💻"))
  assert_eq_string("Hello, 🏳️‍🌈!", azimuth::greet("🏳️‍🌈"))
  
  // 特殊空白字符
  assert_eq_string("Hello, \t\n!", azimuth::greet("\t\n"))
}

test "performance_and_stress_tests" {
  // 测试性能和压力情况
  // 大数运算
  let large1 = 1000000
  let large2 = 2000000
  let large3 = 3000000
  
  assert_eq(6000000, azimuth::add(azimuth::add(large1, large2), large3))
  assert_eq(6000000000000000, azimuth::multiply(large1, azimuth::multiply(large2, large3)))
  
  // 重复运算链
  let chain_result = azimuth::add(1, 1)
  chain_result = azimuth::add(chain_result, 2)
  chain_result = azimuth::add(chain_result, 3)
  chain_result = azimuth::add(chain_result, 4)
  chain_result = azimuth::add(chain_result, 5)
  
  assert_eq(16, chain_result)  // 1+1+2+3+4+5 = 16
}