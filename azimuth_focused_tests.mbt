// Azimuth 项目专注测试用例
// 使用标准 MoonBit 测试语法，专注于核心功能测试

use azimuth

test "add_function_basic" {
  // 测试 add 函数的基本功能
  @assertion.assert_eq(5, azimuth.add(2, 3))?
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  @assertion.assert_eq(-1, azimuth.add(2, -3))?
  @assertion.assert_eq(-10, azimuth.add(-5, -5))?
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  @assertion.assert_eq(6, azimuth.multiply(2, 3))?
  @assertion.assert_eq(0, azimuth.multiply(5, 0))?
  @assertion.assert_eq(-6, azimuth.multiply(2, -3))?
  @assertion.assert_eq(15, azimuth.multiply(-3, -5))?
}

test "greet_function_various_names" {
  // 测试 greet 函数处理各种名称
  @assertion.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))?
  @assertion.assert_eq_string("Hello, Bob!", azimuth.greet("Bob"))?
  @assertion.assert_eq_string("Hello,!", azimuth.greet(""))?
  @assertion.assert_eq_string("Hello, 张三!", azimuth.greet("张三"))?
}

test "arithmetic_combinations" {
  // 测试算术运算组合
  let a = 10
  let b = 3
  let c = 5
  
  // 组合运算：(a + b) * c
  let result1 = azimuth.multiply(azimuth.add(a, b), c)
  @assertion.assert_eq(65, result1)?
  
  // 组合运算：a * b + c
  let result2 = azimuth.add(azimuth.multiply(a, b), c)
  @assertion.assert_eq(35, result2)?
  
  // 组合运算：a + b * c
  let result3 = azimuth.add(a, azimuth.multiply(b, c))
  @assertion.assert_eq(25, result3)?
}

test "zero_operations" {
  // 测试零值运算
  let test_values = [0, 1, -1, 10, -10]
  
  // 测试加法零值
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  @assertion.assert_eq(1, azimuth.add(0, 1))?
  @assertion.assert_eq(-1, azimuth.add(0, -1))?
  @assertion.assert_eq(10, azimuth.add(0, 10))?
  @assertion.assert_eq(-10, azimuth.add(0, -10))?
  
  // 测试乘法零值
  @assertion.assert_eq(0, azimuth.multiply(0, 0))?
  @assertion.assert_eq(0, azimuth.multiply(0, 1))?
  @assertion.assert_eq(0, azimuth.multiply(0, -1))?
  @assertion.assert_eq(0, azimuth.multiply(0, 10))?
  @assertion.assert_eq(0, azimuth.multiply(0, -10))?
}

test "identity_operations" {
  // 测试单位元运算
  let test_values = [1, 2, 5, 10, 100]
  
  // 加法单位元：加0
  @assertion.assert_eq(1, azimuth.add(1, 0))?
  @assertion.assert_eq(2, azimuth.add(2, 0))?
  @assertion.assert_eq(5, azimuth.add(5, 0))?
  @assertion.assert_eq(10, azimuth.add(10, 0))?
  @assertion.assert_eq(100, azimuth.add(100, 0))?
  
  // 乘法单位元：乘1
  @assertion.assert_eq(1, azimuth.multiply(1, 1))?
  @assertion.assert_eq(2, azimuth.multiply(2, 1))?
  @assertion.assert_eq(5, azimuth.multiply(5, 1))?
  @assertion.assert_eq(10, azimuth.multiply(10, 1))?
  @assertion.assert_eq(100, azimuth.multiply(100, 1))?
}

test "negative_number_operations" {
  // 测试负数运算
  let positive = 5
  let negative = -3
  
  // 加法与负数
  @assertion.assert_eq(2, azimuth.add(positive, negative))?
  @assertion.assert_eq(-8, azimuth.add(negative, -5))?
  @assertion.assert_eq(-6, azimuth.add(-3, -3))?
  
  // 乘法与负数
  @assertion.assert_eq(-15, azimuth.multiply(positive, negative))?
  @assertion.assert_eq(15, azimuth.multiply(negative, -5))?
  @assertion.assert_eq(9, azimuth.multiply(-3, -3))?
}

test "large_number_operations" {
  // 测试大数运算
  let large1 = 10000
  let large2 = 20000
  let large3 = 500
  
  // 大数加法
  @assertion.assert_eq(30000, azimuth.add(large1, large2))?
  @assertion.assert_eq(10500, azimuth.add(large1, large3))?
  @assertion.assert_eq(20500, azimuth.add(large2, large3))?
  
  // 大数乘法
  @assertion.assert_eq(200000000, azimuth.multiply(large1, large2))?
  @assertion.assert_eq(5000000, azimuth.multiply(large1, large3))?
  @assertion.assert_eq(10000000, azimuth.multiply(large2, large3))?
}

test "sequential_operations" {
  // 测试连续运算
  let start = 10
  
  // 连续加法
  let result1 = azimuth.add(azimuth.add(azimuth.add(start, 1), 2), 3)
  @assertion.assert_eq(16, result1)?
  
  // 连续乘法
  let result2 = azimuth.multiply(azimuth.multiply(azimuth.multiply(2, 3), 4), 5)
  @assertion.assert_eq(120, result2)?
  
  // 混合连续运算
  let result3 = azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5))
  @assertion.assert_eq(26, result3)?
}

test "edge_case_values" {
  // 测试边界情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法
  @assertion.assert_eq(max_int, azimuth.add(max_int, 0))?
  @assertion.assert_eq(min_int, azimuth.add(min_int, 0))?
  @assertion.assert_eq(-1, azimuth.add(max_int, min_int))?
  
  // 边界值乘法
  @assertion.assert_eq(0, azimuth.multiply(max_int, 0))?
  @assertion.assert_eq(0, azimuth.multiply(min_int, 0))?
  @assertion.assert_eq(max_int, azimuth.multiply(max_int, 1))?
  @assertion.assert_eq(min_int, azimuth.multiply(min_int, 1))?
}