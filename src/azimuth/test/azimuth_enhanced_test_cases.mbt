// Azimuth 项目增强测试用例
// 覆盖更多边界条件和特殊场景

test "add_large_numbers_overflow_check" {
  // 测试大数相加的溢出检查
  let large_num1 = 1000000000
  let large_num2 = 1000000000
  let result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(result, 2000000000)
}

test "multiply_large_numbers" {
  // 测试大数相乘
  let large_num1 = 10000
  let large_num2 = 10000
  let result = @azimuth.multiply(large_num1, large_num2)
  @azimuth.assert_eq(result, 100000000)
}

test "greet_unicode_characters" {
  // 测试 Unicode 字符处理
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, Café!", @azimuth.greet("Café"))
}

test "sequential_operations" {
  // 测试连续运算
  let start = 10
  let step1 = @azimuth.add(start, 5)  // 15
  let step2 = @azimuth.multiply(step1, 2)  // 30
  let step3 = @azimuth.add(step2, -10)  // 20
  let result = @azimuth.multiply(step3, 3)  // 60
  
  @azimuth.assert_eq(result, 60)
}

test "add_associative_property" {
  // 测试加法结合律
  let a = 15
  let b = 25
  let c = 35
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律
  let a = 5
  let b = 10
  let c = 15
  
  let result1 = @azimuth.multiply(a, @azimuth.add(b, c))
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(result1, result2)
}

test "greet_whitespace_handling" {
  // 测试空白字符处理
  @azimuth.assert_eq_string("Hello,   spaced  !", @azimuth.greet("  spaced  "))
  @azimuth.assert_eq_string("Hello, \t tabbed \t!", @azimuth.greet("\t tabbed \t"))
  @azimuth.assert_eq_string("Hello, \n newlined \n!", @azimuth.greet("\n newlined \n"))
}

test "extreme_boundary_values" {
  // 测试极值边界
  let near_max = 2147483640
  let near_min = -2147483640
  
  let result1 = @azimuth.add(near_max, 7)
  @azimuth.assert_eq(result1, 2147483647)
  
  let result2 = @azimuth.add(near_min, -8)
  @azimuth.assert_eq(result2, -2147483648)
}

test "complex_string_patterns" {
  // 测试复杂字符串模式
  @azimuth.assert_eq_string("Hello, 123-456-7890!", @azimuth.greet("123-456-7890"))
  @azimuth.assert_eq_string("Hello, path/to/file.txt!", @azimuth.greet("path/to/file.txt"))
  @azimuth.assert_eq_string("Hello, http://example.com!", @azimuth.greet("http://example.com"))
}