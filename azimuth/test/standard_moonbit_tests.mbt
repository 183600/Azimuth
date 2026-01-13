// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

// 测试用例 1: 基本加法运算
test "basic_addition" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
}

// 测试用例 2: 基本乘法运算
test "basic_multiplication" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(20, @azimuth.multiply(4, 5))
}

// 测试用例 3: 加法边界值测试
test "addition_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

// 测试用例 4: 乘法边界值测试
test "multiplication_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试最小值乘以-1的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 测试用例 5: 字符串问候功能测试
test "greet_functionality" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

// 测试用例 6: 负数运算测试
test "negative_number_operations" {
  // 负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-3, @azimuth.add(-8, 5))
  
  // 负数乘法
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

// 测试用例 7: 数学恒等式测试
test "mathematical_identities" {
  // 加法恒等式
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 乘法恒等式
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  
  // 负数恒等式
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
}

// 测试用例 8: 复合表达式测试
test "compound_expressions" {
  // 嵌套运算
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)
  
  // 多层嵌套
  let result2 = @azimuth.multiply(@azimuth.add(10, 5), @azimuth.add(3, 2))
  @azimuth.assert_eq(75, result2)
  
  // 混合正负数
  let result3 = @azimuth.add(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(-26, result3)
}

// 测试用例 9: 特殊值处理测试
test "special_value_handling" {
  // 测试大数运算
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 测试连续运算
  let start = 100
  let step1 = @azimuth.add(start, 20)  // 120
  let step2 = @azimuth.multiply(step1, 2)  // 240
  let step3 = @azimuth.add(step2, -40)  // 200
  @azimuth.assert_eq(200, step3)
  
  // 测试零的特殊处理
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 999999))
  @azimuth.assert_eq(0, @azimuth.multiply(999999, 0))
}

// 测试用例 10: 字符串边界情况测试
test "string_boundary_cases" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, 	\n!", @azimuth.greet("\t\n"))
  @azimuth.assert_eq_string("Hello, \"quotes\"!", @azimuth.greet("\"quotes\""))
  @azimuth.assert_eq_string("Hello, \\backslash!", @azimuth.greet("\\backslash"))
  
  // 测试长字符串
  let long_string = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let expected = "Hello, " + long_string + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_string))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello,   multiple   spaces   !", @azimuth.greet("  multiple   spaces  "))
}