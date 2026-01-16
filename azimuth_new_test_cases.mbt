// Azimuth 项目新测试用例
// 使用标准 MoonBit 测试语法，覆盖更多场景

test "add_function_associative_property" {
  // 测试加法结合律
  let a = 5
  let b = 10
  let c = 15
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "multiply_function_associative_property" {
  // 测试乘法结合律
  let a = 2
  let b = 3
  let c = 4
  
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "multiply_function_distributive_property" {
  // 测试乘法对加法的分配律
  let a = 3
  let b = 4
  let c = 5
  
  let result1 = @azimuth.multiply(a, @azimuth.add(b, c))
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(result1, result2)
}

test "greet_function_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, @Azimuth!", @azimuth.greet("@Azimuth"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, Azimuth-Test!", @azimuth.greet("Azimuth-Test"))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  
  // 计算平均值: (a + b + c) / 3
  let a = 12
  let b = 18
  let c = 24
  let sum = @azimuth.add(@azimuth.add(a, b), c)
  let average = sum / 3
  @azimuth.assert_eq(18, average)
  
  // 计算平方差: a² - b² = (a + b) * (a - b)
  let x = 10
  let y = 6
  let sum_ab = @azimuth.add(x, y)
  let diff_ab = x - y  // 使用原生减法
  let square_diff = @azimuth.multiply(sum_ab, diff_ab)
  @azimuth.assert_eq(64, square_diff)
}

test "identity_elements" {
  // 测试单位元
  let test_value = 42
  
  // 加法单位元
  @azimuth.assert_eq(test_value, @azimuth.add(test_value, 0))
  @azimuth.assert_eq(test_value, @azimuth.add(0, test_value))
  
  // 乘法单位元
  @azimuth.assert_eq(test_value, @azimuth.multiply(test_value, 1))
  @azimuth.assert_eq(test_value, @azimuth.multiply(1, test_value))
}

test "inverse_elements" {
  // 测试逆元
  let test_value = 17
  
  // 加法逆元
  @azimuth.assert_eq(0, @azimuth.add(test_value, -test_value))
  @azimuth.assert_eq(0, @azimuth.add(-test_value, test_value))
}

test "large_number_operations" {
  // 大数运算测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num1, large_num2))
}

test "greet_function_with_whitespace" {
  // 测试包含空格的问候
  @azimuth.assert_eq_string("Hello, Moon Bit!", @azimuth.greet("Moon Bit"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, leading space!", @azimuth.greet(" leading space"))
  @azimuth.assert_eq_string("Hello, trailing space !", @azimuth.greet("trailing space "))
}