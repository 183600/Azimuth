// 综合 MoonBit 测试用例 - 覆盖核心功能和边界情况
// 导入 azimuth 包以使用其中的函数

test "addition_comprehensive_test" {
  // 测试加法的各种情况
  // 基本加法
  assert_eq(10, azimuth::add(7, 3))
  assert_eq(0, azimuth::add(5, -5))
  assert_eq(-10, azimuth::add(-7, -3))
  
  // 与0相加
  assert_eq(5, azimuth::add(5, 0))
  assert_eq(5, azimuth::add(0, 5))
  assert_eq(0, azimuth::add(0, 0))
  
  // 大数相加
  assert_eq(2000000, azimuth::add(1000000, 1000000))
  assert_eq(-2000000, azimuth::add(-1000000, -1000000))
}

test "multiplication_comprehensive_test" {
  // 测试乘法的各种情况
  // 基本乘法
  assert_eq(15, azimuth::multiply(3, 5))
  assert_eq(-12, azimuth::multiply(-3, 4))
  assert_eq(12, azimuth::multiply(-3, -4))
  
  // 与0和1相乘
  assert_eq(0, azimuth::multiply(8, 0))
  assert_eq(8, azimuth::multiply(8, 1))
  assert_eq(8, azimuth::multiply(1, 8))
  
  // 大数相乘
  assert_eq(1000000, azimuth::multiply(1000, 1000))
  assert_eq(-1000000, azimuth::multiply(-1000, 1000))
}

test "boundary_value_test" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(max_val, -1))
  
  // 最小值运算
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  
  // 极值相加
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "overflow_protection_test" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, 100))
  assert_eq(max_val, azimuth::add(max_val, max_val))
  
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, -100))
  assert_eq(min_val, azimuth::add(min_val, min_val))
  
  // 乘法溢出保护
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, 2))
  assert_eq(max_val, azimuth::multiply(46341, 46341))  // sqrt(max_val)的平方
}

test "string_processing_test" {
  // 测试字符串处理
  // 基本字符串
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  
  // 特殊字符
  assert_eq_string("Hello, @#$%!", azimuth::greet("@#$%"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  
  // Unicode字符
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
  
  // 空字符串和空格
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello,  !", azimuth::greet(" "))
}

test "function_composition_test" {
  // 测试函数组合
  // 复合计算：(a+b)×(c-d)
  let a = 8
  let b = 2
  let c = 15
  let d = 5
  
  let sum_ab = azimuth::add(a, b)
  let diff_cd = azimuth::add(c, -d)
  let result = azimuth::multiply(sum_ab, diff_cd)
  
  assert_eq(100, result)  // (8+2)×(15-5) = 10×10 = 100
  
  // 链式计算：a×b + c×d
  let e = 3
  let f = 4
  let g = 5
  let h = 6
  
  let product1 = azimuth::multiply(e, f)
  let product2 = azimuth::multiply(g, h)
  let sum_result = azimuth::add(product1, product2)
  
  assert_eq(42, sum_result)  // 3×4 + 5×6 = 12 + 30 = 42
}

test "real_world_application_test" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = azimuth::multiply(item1_price, item1_quantity)
  let total2 = azimuth::multiply(item2_price, item2_quantity)
  let cart_total = azimuth::add(total1, total2)
  
  assert_eq(697, cart_total)  // 199×2 + 299×1 = 398 + 299 = 697
  
  // 面积计算
  let length = 12
  let width = 8
  let area = azimuth::multiply(length, width)
  
  assert_eq(96, area)  // 12×8 = 96
  
  // 周长计算
  let perimeter = azimuth::multiply(azimuth::add(length, width), 2)
  assert_eq(40, perimeter)  // (12+8)×2 = 40
}

test "conditional_logic_test" {
  // 测试条件逻辑
  let x = 15
  let y = 20
  
  // 条件：如果x<y则相加，否则相乘
  let condition = x < y
  let result = if condition { azimuth::add(x, y) } else { azimuth::multiply(x, y) }
  assert_eq(35, result)  // 15<20为真，所以15+20=35
  
  // 反向条件
  let result2 = if x > y { azimuth::add(x, y) } else { azimuth::multiply(x, y) }
  assert_eq(300, result2)  // 15>20为假，所以15×20=300
  
  // 嵌套条件
  let z = 10
  let complex_result = 
    if x < y {
      if y < z { azimuth::add(x, azimuth::add(y, z)) } 
      else { azimuth::multiply(x, azimuth::add(y, z)) }
    } else {
      azimuth::add(azimuth::multiply(x, y), z)
    }
  
  assert_eq(350, complex_result)  // 15<20为真，20<10为假，所以15×(20+10)=15×30=450
}

test "mathematical_series_test" {
  // 测试数学数列
  // 等差数列前5项和：2+4+6+8+10 = 30
  let sum1 = azimuth::add(azimuth::add(2, 4), azimuth::add(6, azimuth::add(8, 10)))
  assert_eq(30, sum1)
  
  // 等比数列前4项积：2×4×8×16 = 1024
  let product1 = azimuth::multiply(2, azimuth::multiply(4, azimuth::multiply(8, 16)))
  assert_eq(1024, product1)
  
  // 平方和：1²+2²+3²+4² = 1+4+9+16 = 30
  let square1 = azimuth::multiply(1, 1)
  let square2 = azimuth::multiply(2, 2)
  let square3 = azimuth::multiply(3, 3)
  let square4 = azimuth::multiply(4, 4)
  let sum_of_squares = azimuth::add(azimuth::add(azimuth::add(square1, square2), square3), square4)
  assert_eq(30, sum_of_squares)
}

test "performance_test" {
  // 测试性能相关的大数运算
  // 大数加法
  let large1 = 1000000
  let large2 = 2000000
  let large3 = 3000000
  
  assert_eq(6000000, azimuth::add(azimuth::add(large1, large2), large3))
  
  // 大数乘法
  assert_eq(6000000000000000, azimuth::multiply(large1, azimuth::multiply(large2, large3)))
  
  // 重复运算链
  let chain_result = azimuth::add(1, 1)
  chain_result = azimuth::add(chain_result, 2)
  chain_result = azimuth::add(chain_result, 3)
  chain_result = azimuth::add(chain_result, 4)
  chain_result = azimuth::add(chain_result, 5)
  
  assert_eq(16, chain_result)  // 1+1+2+3+4+5 = 16
  
  // 嵌套运算
  let nested_result = azimuth::multiply(
    azimuth::add(100, 200), 
    azimuth::add(azimuth::multiply(3, 4), azimuth::multiply(5, 6))
  )
  assert_eq(12600, nested_result)  // (100+200)×(3×4+5×6) = 300×(12+30) = 300×42 = 12600
}