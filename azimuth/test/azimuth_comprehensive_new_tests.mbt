// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "add_function_edge_cases" {
  // 测试add函数的边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
}

test "multiply_function_edge_cases" {
  // 测试multiply函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-1, 12345))
}

test "greet_function_variations" {
  // 测试greet函数的各种情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

test "add_function_negative_numbers" {
  // 测试add函数的负数处理
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-15, @azimuth.add(-20, 5))
}

test "multiply_function_negative_numbers" {
  // 测试multiply函数的负数处理
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let a = 10
  let b = 20
  let c = 30
  
  let sum_ab = @azimuth.add(a, b)
  let product_bc = @azimuth.multiply(b, c)
  let final_result = @azimuth.add(sum_ab, product_bc)
  
  @azimuth.assert_eq(30, sum_ab)
  @azimuth.assert_eq(600, product_bc)
  @azimuth.assert_eq(630, final_result)
}

test "overflow_handling_addition" {
  // 测试add函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 这些测试验证溢出处理是否正确
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "overflow_handling_multiplication" {
  // 测试multiply函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 这些测试验证溢出处理是否正确
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "string_greeting_with_special_characters" {
  // 测试包含特殊字符的字符串问候
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "sequential_operations_with_different_types" {
  // 测试不同类型的连续操作
  let num1 = 100
  let num2 = 50
  let name = "Azimuth"
  
  let sum = @azimuth.add(num1, num2)
  let product = @azimuth.multiply(sum, 2)
  let greeting = @azimuth.greet(name)
  
  @azimuth.assert_eq(150, sum)
  @azimuth.assert_eq(300, product)
  @azimuth.assert_eq_string("Hello, Azimuth!", greeting)
}