// 新的综合测试用例 - 使用标准 MoonBit 测试语法
// 测试 azimuth 包的各种功能和边界情况

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(add(5, 3), add(3, 5))
  assert_eq(add(-5, 3), add(3, -5))
  assert_eq(add(-5, -3), add(-3, -5))
  assert_eq(add(0, 100), add(100, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
  assert_eq(multiply(3, add(5, 2)), add(multiply(3, 5), multiply(3, 2)))
  assert_eq(multiply(-2, add(3, 4)), add(multiply(-2, 3), multiply(-2, 4)))
}

test "divide_with_ceil_fractional_behavior" {
  // 测试向上取整除法的小数行为
  // 正数情况
  assert_eq(2, divide_with_ceil(4, 2))  // 整除
  assert_eq(3, divide_with_ceil(5, 2))  // 2.5 向上取整
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 向上取整
  
  // 负数情况
  assert_eq(-2, divide_with_ceil(-4, 2))  // 整除
  assert_eq(-2, divide_with_ceil(-5, 2))  // -2.5 向上取整
  assert_eq(-1, divide_with_ceil(-3, 2))  // -1.5 向上取整
}

test "greet_function_empty_and_whitespace" {
  // 测试 greet 函数的空字符串和空白字符处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,    !", greet("   "))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
}

test "add_function_identity_elements" {
  // 测试加法的单位元
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, add(0, 0))
  assert_eq(2147483647, add(2147483647, 0))
  assert_eq(-2147483648, add(-2147483648, 0))
}

test "multiply_function_identity_elements" {
  // 测试乘法的单位元
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(-5, 1))
  assert_eq(0, multiply(0, 1))
  assert_eq(2147483647, multiply(2147483647, 1))
  assert_eq(-2147483648, multiply(-2147483648, 1))
}

test "complex_nested_operations" {
  // 测试复杂的嵌套运算
  // 多层嵌套加法和乘法
  let result1 = add(multiply(2, add(3, 4)), multiply(5, add(1, 2)))
  assert_eq(27, result1)  // 2*(3+4) + 5*(1+2) = 2*7 + 5*3 = 14 + 15 = 29
  
  let result2 = multiply(add(2, 3), add(4, 5))
  assert_eq(45, result2)  // (2+3)*(4+5) = 5*9 = 45
  
  // 包含除法的复杂运算
  let result3 = add(multiply(2, 3), divide_with_ceil(multiply(4, 5), 3))
  assert_eq(15, result3)  // 2*3 + ceil(4*5/3) = 6 + ceil(20/3) = 6 + 7 = 13
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边界情况
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC!", greet("ABC"))
  assert_eq_string("Hello, abc123!", greet("abc123"))
  assert_eq_string("Hello, !@#$%^&*()!", greet("!@#$%^&*()"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "mathematical_invariants" {
  // 测试数学不变量
  let x = 42
  let y = 17
  
  // 加法不变量
  assert_eq(x, add(add(x, y), -y))
  assert_eq(y, add(add(x, y), -x))
  
  // 乘法不变量
  assert_eq(x, multiply(multiply(x, y), divide_with_ceil(y, y)))  // x*y/y = x
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, y))
}

test "performance_consistency" {
  // 测试性能一致性（多次运算结果相同）
  let a = 12345
  let b = 67890
  
  // 多次执行相同运算，结果应该一致
  let result1 = add(a, b)
  let result2 = add(a, b)
  let result3 = add(a, b)
  assert_eq(result1, result2)
  assert_eq(result2, result3)
  
  let mul_result1 = multiply(a, 2)
  let mul_result2 = multiply(a, 2)
  let mul_result3 = multiply(a, 2)
  assert_eq(mul_result1, mul_result2)
  assert_eq(mul_result2, mul_result3)
  
  let div_result1 = divide_with_ceil(a, 10)
  let div_result2 = divide_with_ceil(a, 10)
  let div_result3 = divide_with_ceil(a, 10)
  assert_eq(div_result1, div_result2)
  assert_eq(div_result2, div_result3)
}