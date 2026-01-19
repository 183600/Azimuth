// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法，提供全面的测试覆盖

// 导入必要的模块
use azimuth

test "addition_function_basic" {
  // 测试加法函数的基本功能
  @assertion.assert_eq(10, azimuth.add(3, 7))?
  @assertion.assert_eq(0, azimuth.add(-5, 5))?
  @assertion.assert_eq(-8, azimuth.add(-3, -5))?
  @assertion.assert_eq(100, azimuth.add(50, 50))?
}

test "multiplication_function_basic" {
  // 测试乘法函数的基本功能
  @assertion.assert_eq(15, azimuth.multiply(3, 5))?
  @assertion.assert_eq(0, azimuth.multiply(0, 123))?
  @assertion.assert_eq(25, azimuth.multiply(-5, -5))?
  @assertion.assert_eq(-20, azimuth.multiply(4, -5))?
}

test "greet_function_with_various_names" {
  // 测试问候函数与各种名称
  @assertion.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))?
  @assertion.assert_eq_string("Hello, 测试!", azimuth.greet("测试"))?
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
}

test "combined_operations" {
  // 测试组合运算
  let a = 10
  let b = 5
  
  // 先加法后乘法
  let sum = azimuth.add(a, b)
  let product = azimuth.multiply(sum, 2)
  @assertion.assert_eq(30, product)?
  
  // 先乘法后加法
  let product2 = azimuth.multiply(a, b)
  let sum2 = azimuth.add(product2, 10)
  @assertion.assert_eq(60, sum2)?
}

test "edge_cases_with_zeros" {
  // 测试零的边界情况
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  @assertion.assert_eq(0, azimuth.multiply(0, 0))?
  @assertion.assert_eq(123, azimuth.add(123, 0))?
  @assertion.assert_eq(-456, azimuth.add(-456, 0))?
}

test "large_number_operations" {
  // 测试大数运算
  let large1 = 100000
  let large2 = 200000
  
  @assertion.assert_eq(300000, azimuth.add(large1, large2))?
  @assertion.assert_eq(20000000000, azimuth.multiply(large1, large2))?
}

test "negative_number_combinations" {
  // 测试负数组合
  @assertion.assert_eq(-15, azimuth.add(-10, -5))?
  @assertion.assert_eq(5, azimuth.add(-10, 15))?
  @assertion.assert_eq(50, azimuth.multiply(-10, -5))?
  @assertion.assert_eq(-30, azimuth.multiply(6, -5))?
}

test "sequential_calculation_pattern" {
  // 测试连续计算模式
  let result = azimuth.add(1, 2)
  result = azimuth.add(result, 3)
  result = azimuth.add(result, 4)
  result = azimuth.multiply(result, 2)
  
  @assertion.assert_eq(20, result)?
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let x = 42
  
  // 加法恒等：x + 0 = x
  @assertion.assert_eq(x, azimuth.add(x, 0))?
  
  // 乘法恒等：x * 1 = x（假设有乘法函数）
  @assertion.assert_eq(x, azimuth.multiply(x, 1))?
  
  // 乘法零律：x * 0 = 0
  @assertion.assert_eq(0, azimuth.multiply(x, 0))?
}