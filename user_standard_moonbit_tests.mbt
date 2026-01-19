// 用户标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，不超过10个测试用例

test "basic_addition_functionality" {
  // 测试基本加法功能
  let result1 = add(5, 3)
  let result2 = add(-2, 7)
  let result3 = add(0, 10)
  let result4 = add(-5, -3)
  
  @assertion.assert_eq(8, result1)?
  @assertion.assert_eq(5, result2)?
  @assertion.assert_eq(10, result3)?
  @assertion.assert_eq(-8, result4)?
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  let result1 = multiply(4, 6)
  let result2 = multiply(-3, 5)
  let result3 = multiply(0, 8)
  let result4 = multiply(-2, -4)
  
  @assertion.assert_eq(24, result1)?
  @assertion.assert_eq(-15, result2)?
  @assertion.assert_eq(0, result3)?
  @assertion.assert_eq(8, result4)?
}

test "greet_function_with_various_inputs" {
  // 测试greet函数的各种输入
  let result1 = greet("Alice")
  let result2 = greet("Bob")
  let result3 = greet("")
  let result4 = greet("张三")
  
  @assertion.assert_eq_string("Hello, Alice!", result1)?
  @assertion.assert_eq_string("Hello, Bob!", result2)?
  @assertion.assert_eq_string("Hello, !", result3)?
  @assertion.assert_eq_string("Hello, 张三!", result4)?
}

test "mathematical_operations_combined" {
  // 测试数学运算的组合
  let a = 10
  let b = 3
  let c = 5
  
  // (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  
  @assertion.assert_eq(65, result)?
}

test "edge_cases_with_zero_and_one" {
  // 测试0和1的边界情况
  @assertion.assert_eq(5, add(5, 0))?
  @assertion.assert_eq(5, add(0, 5))?
  @assertion.assert_eq(0, multiply(5, 0))?
  @assertion.assert_eq(0, multiply(0, 5))?
  @assertion.assert_eq(5, multiply(5, 1))?
  @assertion.assert_eq(5, multiply(1, 5))?
}

test "large_numbers_handling" {
  // 测试大数处理
  let large1 = 1000000
  let large2 = 2000000
  
  let sum_result = add(large1, large2)
  let mul_result = multiply(1000, 1000)
  
  @assertion.assert_eq(3000000, sum_result)?
  @assertion.assert_eq(1000000, mul_result)?
}

test "negative_numbers_operations" {
  // 测试负数运算
  let result1 = add(-10, -5)
  let result2 = add(-10, 5)
  let result3 = multiply(-4, 3)
  let result4 = multiply(-4, -3)
  
  @assertion.assert_eq(-15, result1)?
  @assertion.assert_eq(-5, result2)?
  @assertion.assert_eq(-12, result3)?
  @assertion.assert_eq(12, result4)?
}

test "string_operations_with_special_characters" {
  // 测试包含特殊字符的字符串操作
  let result1 = greet("test@example.com")
  let result2 = greet("user_123")
  let result3 = greet("Hello World")
  
  @assertion.assert_eq_string("Hello, test@example.com!", result1)?
  @assertion.assert_eq_string("Hello, user_123!", result2)?
  @assertion.assert_eq_string("Hello, Hello World!", result3)?
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  
  @assertion.assert_eq(40, area)?
  @assertion.assert_eq(26, perimeter)?
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  @assertion.assert_eq(max_val, add(max_val, 0))?
  @assertion.assert_eq(min_val, add(min_val, 0))?
  
  // 测试边界值乘法
  @assertion.assert_eq(0, multiply(max_val, 0))?
  @assertion.assert_eq(0, multiply(min_val, 0))?
}