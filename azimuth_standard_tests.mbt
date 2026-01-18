// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供全面的测试覆盖

// 导入必要的模块
use azimuth

test "string_greeting_functionality" {
  // 测试字符串问候功能
  let result1 = azimuth.greet("Alice")
  @assertion.assert_eq_string("Hello, Alice!", result1)?
  
  let result2 = azimuth.greet("Bob")
  @assertion.assert_eq_string("Hello, Bob!", result2)?
  
  let result3 = azimuth.greet("")
  @assertion.assert_eq_string("Hello, !", result3)?
}

test "basic_addition_operations" {
  // 测试基本加法运算
  @assertion.assert_eq(5, azimuth.add(2, 3))?
  @assertion.assert_eq(0, azimuth.add(-2, 2))?
  @assertion.assert_eq(-5, azimuth.add(-2, -3))?
  @assertion.assert_eq(100, azimuth.add(50, 50))?
}

test "basic_multiplication_operations" {
  // 测试基本乘法运算
  @assertion.assert_eq(6, azimuth.multiply(2, 3))?
  @assertion.assert_eq(0, azimuth.multiply(5, 0))?
  @assertion.assert_eq(-6, azimuth.multiply(-2, 3))?
  @assertion.assert_eq(6, azimuth.multiply(-2, -3))?
}

test "mathematical_expressions" {
  // 测试数学表达式组合
  let a = 5
  let b = 3
  let c = 2
  
  // 测试 (a + b) * c
  let result1 = azimuth.multiply(azimuth.add(a, b), c)
  @assertion.assert_eq(16, result1)?
  
  // 测试 a * b + c
  let result2 = azimuth.add(azimuth.multiply(a, b), c)
  @assertion.assert_eq(17, result2)?
}

test "boundary_value_testing" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试最大值加法
  @assertion.assert_eq(max_int, azimuth.add(max_int, 0))?
  @assertion.assert_eq(min_int, azimuth.add(min_int, 0))?
  
  // 测试最大值乘法
  @assertion.assert_eq(0, azimuth.multiply(max_int, 0))?
  @assertion.assert_eq(0, azimuth.multiply(min_int, 0))?
}

test "sequence_calculations" {
  // 测试序列计算
  // 计算等差数列: 1, 3, 5, 7, 9 的和
  let term1 = 1
  let term2 = azimuth.add(term1, 2)
  let term3 = azimuth.add(term2, 2)
  let term4 = azimuth.add(term3, 2)
  let term5 = azimuth.add(term4, 2)
  
  let sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(term1, term2), term3), term4), term5)
  @assertion.assert_eq(25, sum)?
}

test "business_calculations" {
  // 测试商业计算场景
  let unit_price = 100
  let quantity = 5
  let discount_rate = 10  // 10%
  
  // 计算小计
  let subtotal = azimuth.multiply(unit_price, quantity)
  @assertion.assert_eq(500, subtotal)?
  
  // 计算折扣金额
  let discount_amount = azimuth.multiply(subtotal, discount_rate) / 100
  @assertion.assert_eq(50, discount_amount)?
  
  // 计算最终价格
  let final_price = azimuth.add(subtotal, -discount_amount)
  @assertion.assert_eq(450, final_price)?
}

test "geometry_calculations" {
  // 测试几何计算
  let length = 10
  let width = 6
  
  // 计算面积
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(60, area)?
  
  // 计算周长
  let perimeter = azimuth.multiply(azimuth.add(length, width), 2)
  @assertion.assert_eq(32, perimeter)?
}

test "algorithm_simulation" {
  // 测试算法模拟
  // 模拟简单排序的比较次数
  let array_size = 5
  
  // 冒泡排序最坏情况比较次数: n*(n-1)/2
  let comparisons = azimuth.multiply(array_size, azimuth.add(array_size, -1)) / 2
  @assertion.assert_eq(10, comparisons)?
  
  // 模拟二分查找的最大比较次数: log2(n) 向上取整
  let binary_search_comparisons = 3  // log2(5) ≈ 2.32, 向上取整为3
  @assertion.assert_eq(3, binary_search_comparisons)?
}