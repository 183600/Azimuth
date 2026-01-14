// 增强的 MoonBit 测试用例
// 覆盖更多边界情况和实际应用场景
test "complex_arithmetic_operations" {
  // 测试复合算术运算：(a + b) * c - d
  let a = 15
  let b = 25
  let c = 4
  let d = 30
  
  let sum_ab = azimuth::@azimuth.add(a, b)
  let product = azimuth::@azimuth.multiply(sum_ab, c)
  let result = azimuth::@azimuth.add(product, -d)
  
  azimuth::@azimuth.assert_eq(130, result)
}
test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  azimuth::@azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
  azimuth::@azimuth.assert_eq_string("Hello, A!", azimuth::@azimuth.greet("A"))
  azimuth::@azimuth.assert_eq_string("Hello, 123!", azimuth::@azimuth.greet("123"))
  azimuth::@azimuth.assert_eq_string("Hello, 🌟!", azimuth::@azimuth.greet("🌟"))
}
test "mathematical_series_calculations" {
  // 测试数学数列计算
  // 计算等差数列前n项和：n * (first + last) / 2
  let n = 10
  let first = 1
  let last = 10
  
  let sum = azimuth::@azimuth.multiply(n, azimuth::@azimuth.add(first, last)) / 2
  azimuth::@azimuth.assert_eq(55, sum)
}
test "overflow_protection_mechanisms" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1000))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1000))
  
  // 测试乘法溢出保护
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 2))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, 2))
}
test "real_world_financial_calculations" {
  // 测试实际金融计算场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息计算：本金 + 本金 * 利率 * 年数
  let interest = azimuth::@azimuth.multiply(principal, azimuth::@azimuth.multiply(rate, years))
  let total = azimuth::@azimuth.add(principal, interest)
  
  azimuth::@azimuth.assert_eq(160000, total)
}
test "geometric_calculations" {
  // 测试几何计算
  let length = 12
  let width = 8
  
  // 计算矩形面积
  let area = azimuth::@azimuth.multiply(length, width)
  azimuth::@azimuth.assert_eq(96, area)
  
  // 计算矩形周长：2 * (length + width)
  let perimeter = azimuth::@azimuth.multiply(2, azimuth::@azimuth.add(length, width))
  azimuth::@azimuth.assert_eq(40, perimeter)
}
test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化公式）：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius, 2), 30)
  azimuth::@azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = azimuth::@azimuth.add(azimuth::@azimuth.multiply(0, 2), 30)
  azimuth::@azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = azimuth::@azimuth.add(azimuth::@azimuth.multiply(100, 2), 30)
  azimuth::@azimuth.assert_eq(230, boiling)
}
test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数的正确性
  azimuth::@azimuth.assert_true(true)
  azimuth::@azimuth.assert_false(false)
  azimuth::@azimuth.assert_eq(42, 42)
  azimuth::@azimuth.assert_eq_string("test", "test")
  
  // 测试边界值的正确处理
  azimuth::@azimuth.assert_true(azimuth::@azimuth.add(max_val, 1) == max_val)
  azimuth::@azimuth.assert_true(azimuth::@azimuth.add(min_val, -1) == min_val)
}
test "performance_related_operations" {
  // 测试性能相关的操作
  let base = 100
  let multiplier = 1000
  
  // 测试大数运算
  let large_result = azimuth::@azimuth.multiply(base, multiplier)
  azimuth::@azimuth.assert_eq(100000, large_result)
  
  // 测试连续运算
  let consecutive_result = azimuth::@azimuth.add(azimuth::@azimuth.multiply(10, 20), azimuth::@azimuth.multiply(30, 40))
  azimuth::@azimuth.assert_eq(1400, consecutive_result)
}
test "data_validation_scenarios" {
  // 测试数据验证场景
  // 测试输入验证
  let valid_inputs = [0, 1, -1, 100, -100, 2147483647, -2147483648]
  
  // 验证每个输入与0的加法
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.add(0, 0))
  azimuth::@azimuth.assert_eq(1, azimuth::@azimuth.add(1, 0))
  azimuth::@azimuth.assert_eq(-1, azimuth::@azimuth.add(-1, 0))
  azimuth::@azimuth.assert_eq(100, azimuth::@azimuth.add(100, 0))
  azimuth::@azimuth.assert_eq(-100, azimuth::@azimuth.add(-100, 0))
  azimuth::@azimuth.assert_eq(2147483647, azimuth::@azimuth.add(2147483647, 0))
  azimuth::@azimuth.assert_eq(-2147483648, azimuth::@azimuth.add(-2147483648, 0))
  
  // 验证每个输入与1的乘法
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(0, 1))
  azimuth::@azimuth.assert_eq(1, azimuth::@azimuth.multiply(1, 1))
  azimuth::@azimuth.assert_eq(-1, azimuth::@azimuth.multiply(-1, 1))
  azimuth::@azimuth.assert_eq(100, azimuth::@azimuth.multiply(100, 1))
  azimuth::@azimuth.assert_eq(-100, azimuth::@azimuth.multiply(-100, 1))
  azimuth::@azimuth.assert_eq(2147483647, azimuth::@azimuth.multiply(2147483647, 1))
  azimuth::@azimuth.assert_eq(-2147483648, azimuth::@azimuth.multiply(-2147483648, 1))
}// 综合 MoonBit 测试用例 - 覆盖核心功能和边界情况
// 导入 azimuth 包以使用其中的函数
test "addition_comprehensive_test" {
  // 测试加法的各种情况
  // 基本加法
  @azimuth.assert_eq(10, azimuth::@azimuth.add(7, 3))
  @azimuth.assert_eq(0, azimuth::@azimuth.add(5, -5))
  @azimuth.assert_eq(-10, azimuth::@azimuth.add(-7, -3))
  
  // 与0相加
  @azimuth.assert_eq(5, azimuth::@azimuth.add(5, 0))
  @azimuth.assert_eq(5, azimuth::@azimuth.add(0, 5))
  @azimuth.assert_eq(0, azimuth::@azimuth.add(0, 0))
  
  // 大数相加
  @azimuth.assert_eq(2000000, azimuth::@azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(-2000000, azimuth::@azimuth.add(-1000000, -1000000))
}
test "multiplication_comprehensive_test" {
  // 测试乘法的各种情况
  // 基本乘法
  @azimuth.assert_eq(15, azimuth::@azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, azimuth::@azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, azimuth::@azimuth.multiply(-3, -4))
  
  // 与0和1相乘
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(8, 0))
  @azimuth.assert_eq(8, azimuth::@azimuth.multiply(8, 1))
  @azimuth.assert_eq(8, azimuth::@azimuth.multiply(1, 8))
  
  // 大数相乘
  @azimuth.assert_eq(1000000, azimuth::@azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, azimuth::@azimuth.multiply(-1000, 1000))
}
test "boundary_value_test" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(max_val, -1))
  
  // 最小值运算
  @azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
  
  // 极值相加
  @azimuth.assert_eq(-1, azimuth::@azimuth.add(max_val, min_val))
}
test "overflow_protection_test" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, max_val))
  
  @azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, min_val))
  
  // 乘法溢出保护
  @azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(46341, 46341))  // sqrt(max_val)的平方
}
test "string_processing_test" {
  // 测试字符串处理
  // 基本字符串
  @azimuth.assert_eq_string("Hello, World!", azimuth::@azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", azimuth::@azimuth.greet("MoonBit"))
  
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", azimuth::@azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 123!", azimuth::@azimuth.greet("123"))
  
  // Unicode字符
  @azimuth.assert_eq_string("Hello, 测试!", azimuth::@azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🌍!", azimuth::@azimuth.greet("🌍"))
  
  // 空字符串和空格
  @azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", azimuth::@azimuth.greet(" "))
}
test "function_composition_test" {
  // 测试函数组合
  // 复合计算：(a+b)×(c-d)
  let a = 8
  let b = 2
  let c = 15
  let d = 5
  
  let sum_ab = azimuth::@azimuth.add(a, b)
  let diff_cd = azimuth::@azimuth.add(c, -d)
  let result = azimuth::@azimuth.multiply(sum_ab, diff_cd)
  
  @azimuth.assert_eq(100, result)  // (8+2)×(15-5) = 10×10 = 100
  
  // 链式计算：a×b + c×d
  let e = 3
  let f = 4
  let g = 5
  let h = 6
  
  let product1 = azimuth::@azimuth.multiply(e, f)
  let product2 = azimuth::@azimuth.multiply(g, h)
  let sum_result = azimuth::@azimuth.add(product1, product2)
  
  @azimuth.assert_eq(42, sum_result)  // 3×4 + 5×6 = 12 + 30 = 42
}
test "real_world_application_test" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = azimuth::@azimuth.multiply(item1_price, item1_quantity)
  let total2 = azimuth::@azimuth.multiply(item2_price, item2_quantity)
  let cart_total = azimuth::@azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)  // 199×2 + 299×1 = 398 + 299 = 697
  
  // 面积计算
  let length = 12
  let width = 8
  let area = azimuth::@azimuth.multiply(length, width)
  
  @azimuth.assert_eq(96, area)  // 12×8 = 96
  
  // 周长计算
  let perimeter = azimuth::@azimuth.multiply(azimuth::@azimuth.add(length, width), 2)
  @azimuth.assert_eq(40, perimeter)  // (12+8)×2 = 40
}
test "conditional_logic_test" {
  // 测试条件逻辑
  let x = 15
  let y = 20
  
  // 条件：如果x<y则相加，否则相乘
  let condition = x < y
  let result = if condition { azimuth::@azimuth.add(x, y) } else { azimuth::@azimuth.multiply(x, y) }
  @azimuth.assert_eq(35, result)  // 15<20为真，所以15+20=35
  
  // 反向条件
  let result2 = if x > y { azimuth::@azimuth.add(x, y) } else { azimuth::@azimuth.multiply(x, y) }
  @azimuth.assert_eq(300, result2)  // 15>20为假，所以15×20=300
  
  // 嵌套条件
  let z = 10
  let complex_result = 
    if x < y {
      if y < z { azimuth::@azimuth.add(x, azimuth::@azimuth.add(y, z)) } 
      else { azimuth::@azimuth.multiply(x, azimuth::@azimuth.add(y, z)) }
    } else {
      azimuth::@azimuth.add(azimuth::@azimuth.multiply(x, y), z)
    }
  
  @azimuth.assert_eq(350, complex_result)  // 15<20为真，20<10为假，所以15×(20+10)=15×30=450
}
test "mathematical_series_test" {
  // 测试数学数列
  // 等差数列前5项和：2+4+6+8+10 = 30
  let sum1 = azimuth::@azimuth.add(azimuth::@azimuth.add(2, 4), azimuth::@azimuth.add(6, azimuth::@azimuth.add(8, 10)))
  @azimuth.assert_eq(30, sum1)
  
  // 等比数列前4项积：2×4×8×16 = 1024
  let product1 = azimuth::@azimuth.multiply(2, azimuth::@azimuth.multiply(4, azimuth::@azimuth.multiply(8, 16)))
  @azimuth.assert_eq(1024, product1)
  
  // 平方和：1²+2²+3²+4² = 1+4+9+16 = 30
  let square1 = azimuth::@azimuth.multiply(1, 1)
  let square2 = azimuth::@azimuth.multiply(2, 2)
  let square3 = azimuth::@azimuth.multiply(3, 3)
  let square4 = azimuth::@azimuth.multiply(4, 4)
  let sum_of_squares = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(square1, square2), square3), square4)
  @azimuth.assert_eq(30, sum_of_squares)
}
test "performance_test" {
  // 测试性能相关的大数运算
  // 大数加法
  let large1 = 1000000
  let large2 = 2000000
  let large3 = 3000000
  
  @azimuth.assert_eq(6000000, azimuth::@azimuth.add(azimuth::@azimuth.add(large1, large2), large3))
  
  // 大数乘法
  @azimuth.assert_eq(6000000000000000, azimuth::@azimuth.multiply(large1, azimuth::@azimuth.multiply(large2, large3)))
  
  // 重复运算链
  let chain_result = azimuth::@azimuth.add(1, 1)
  chain_result = azimuth::@azimuth.add(chain_result, 2)
  chain_result = azimuth::@azimuth.add(chain_result, 3)
  chain_result = azimuth::@azimuth.add(chain_result, 4)
  chain_result = azimuth::@azimuth.add(chain_result, 5)
  
  @azimuth.assert_eq(16, chain_result)  // 1+1+2+3+4+5 = 16
  
  // 嵌套运算
  let nested_result = azimuth::@azimuth.multiply(
    azimuth::@azimuth.add(100, 200), 
    azimuth::@azimuth.add(azimuth::@azimuth.multiply(3, 4), azimuth::@azimuth.multiply(5, 6))
  )
  @azimuth.assert_eq(12600, nested_result)  // (100+200)×(3×4+5×6) = 300×(12+30) = 300×42 = 12600
}