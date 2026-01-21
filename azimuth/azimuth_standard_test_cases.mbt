// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "add_function_zero_handling" {
  // 测试加法函数的零值处理
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(-5, add(0, -5))
}

test "multiply_function_negative_values" {
  // 测试乘法函数的负值处理
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, -5))
}

test "greet_function_empty_input" {
  // 测试 greet 函数的空输入处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,   !", greet("  "))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(1, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "combined_operations_calculation" {
  // 测试组合运算
  let a = 8
  let b = 3
  
  // (a + b) * 2
  let result1 = multiply(add(a, b), 2)
  assert_eq(22, result1)
  
  // a * b + a
  let result2 = add(multiply(a, b), a)
  assert_eq(32, result2)
}

test "edge_case_large_numbers" {
  // 测试大数处理的边界情况
  let large_num = 1000000
  let result1 = add(large_num, large_num)
  let result2 = multiply(1000, 1000)
  
  assert_eq(2000000, result1)
  assert_eq(1000000, result2)
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  let left = multiply(x, add(y, 3))
  let right = add(multiply(x, y), multiply(x, 3))
  assert_eq(left, right)
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边界情况
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC!", greet("ABC"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
}

test "division_by_negative_numbers" {
  // 测试负数除法
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(2, divide_with_ceil(-6, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
}

test "complex_business_calculation" {
  // 测试复杂业务计算
  let price = 199
  let quantity = 3
  let tax_rate = 10
  
  // 计算总价
  let subtotal = multiply(price, quantity)
  assert_eq(597, subtotal)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(subtotal, tax_rate)
  assert_eq(60, tax)
  
  // 计算最终总价
  let total = add(subtotal, tax)
  assert_eq(657, total)
}