// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

test "add_basic_functionality" {
  // 基本加法功能测试
  azimuth.assert_eq(5, azimuth.add(2, 3))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-1, azimuth.add(2, -3))
  azimuth.assert_eq(10, azimuth.add(7, 3))
}

test "add_edge_cases" {
  // 加法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值加法
  azimuth.assert_eq(5, azimuth.add(0, 5))
  azimuth.assert_eq(-3, azimuth.add(-3, 0))
  
  // 测试溢出保护
  azimuth.assert_eq(max_val, azimuth.add(max_val, 1))
  azimuth.assert_eq(min_val, azimuth.add(min_val, -1))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  azimuth.assert_eq(6, azimuth.multiply(2, 3))
  azimuth.assert_eq(0, azimuth.multiply(0, 5))
  azimuth.assert_eq(-6, azimuth.multiply(2, -3))
  azimuth.assert_eq(15, azimuth.multiply(3, 5))
}

test "multiply_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试单位元
  azimuth.assert_eq(5, azimuth.multiply(1, 5))
  azimuth.assert_eq(5, azimuth.multiply(5, 1))
  azimuth.assert_eq(-5, azimuth.multiply(-1, 5))
  
  // 测试溢出保护
  azimuth.assert_eq(max_val, azimuth.multiply(max_val, 2))
  azimuth.assert_eq(min_val, azimuth.multiply(min_val, -1))
}

test "greet_functionality" {
  // 问候函数功能测试
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
}

test "mathematical_combinations" {
  // 数学组合运算测试
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result1 = azimuth.multiply(azimuth.add(a, b), c)
  azimuth.assert_eq(45, result1)
  
  // a * b + c
  let result2 = azimuth.add(azimuth.multiply(a, b), c)
  azimuth.assert_eq(53, result2)
}

test "negative_number_operations" {
  // 负数运算测试
  azimuth.assert_eq(-5, azimuth.add(-2, -3))
  azimuth.assert_eq(1, azimuth.add(-2, 3))
  azimuth.assert_eq(6, azimuth.multiply(-2, -3))
  azimuth.assert_eq(-6, azimuth.multiply(2, -3))
}

test "large_number_operations" {
  // 大数运算测试
  let large_num = 1000000
  let another_large = 2000000
  
  azimuth.assert_eq(3000000, azimuth.add(large_num, another_large))
  // 使用较小的数避免溢出
  azimuth.assert_eq(2000000, azimuth.multiply(large_num, 2))
}

test "special_values" {
  // 特殊值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加最小值
  azimuth.assert_eq(-1, azimuth.add(max_val, min_val))
  
  // 最小值乘以1
  azimuth.assert_eq(min_val, azimuth.multiply(min_val, 1))
  
  // 最大值乘以0
  azimuth.assert_eq(0, azimuth.multiply(max_val, 0))
}