// 标准 MoonBit 测试用例补充
// 专注于核心功能的不同测试场景

test "add_function_commutative_property" {
  // 测试加法交换律: a + b = b + a
  // 基本情况
  assert_eq(add(1, 2), add(2, 1))
  assert_eq(add(-1, 1), add(1, -1))
  assert_eq(add(100, 200), add(200, 100))
  assert_eq(add(-50, -25), add(-25, -50))
  
  // 边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(add(max_val, 0), add(0, max_val))
  assert_eq(add(min_val, 0), add(0, min_val))
  
  // 零的情况
  assert_eq(add(0, 0), add(0, 0))
}

test "multiply_function_commutative_property" {
  // 测试乘法交换律: a × b = b × a
  // 基本情况
  assert_eq(multiply(1, 2), multiply(2, 1))
  assert_eq(multiply(-1, 1), multiply(1, -1))
  assert_eq(multiply(10, 20), multiply(20, 10))
  assert_eq(multiply(-5, -3), multiply(-3, -5))
  
  // 边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(multiply(max_val, 1), multiply(1, max_val))
  assert_eq(multiply(min_val, 1), multiply(1, min_val))
  
  // 零的情况
  assert_eq(multiply(0, 1), multiply(1, 0))
}

test "add_function_associative_property" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  // 基本情况
  let left1 = add(add(1, 2), 3)
  let right1 = add(1, add(2, 3))
  assert_eq(left1, right1)
  
  let left2 = add(add(-1, 2), 3)
  let right2 = add(-1, add(2, 3))
  assert_eq(left2, right2)
  
  let left3 = add(add(100, 200), 300)
  let right3 = add(100, add(200, 300))
  assert_eq(left3, right3)
  
  // 负数情况
  let left4 = add(add(-50, 25), 75)
  let right4 = add(-50, add(25, 75))
  assert_eq(left4, right4)
  
  // 零的情况
  let left5 = add(add(0, 0), 0)
  let right5 = add(0, add(0, 0))
  assert_eq(left5, right5)
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a × (b + c) = a × b + a × c
  // 基本情况
  let left1 = multiply(2, add(3, 4))
  let right1 = add(multiply(2, 3), multiply(2, 4))
  assert_eq(left1, right1)
  
  let left2 = multiply(3, add(5, 7))
  let right2 = add(multiply(3, 5), multiply(3, 7))
  assert_eq(left2, right2)
  
  let left3 = multiply(10, add(20, 30))
  let right3 = add(multiply(10, 20), multiply(10, 30))
  assert_eq(left3, right3)
  
  // 包含负数的情况
  let left4 = multiply(5, add(-3, 2))
  let right4 = add(multiply(5, -3), multiply(5, 2))
  assert_eq(left4, right4)
  
  // 零的情况
  let left5 = multiply(0, add(100, 200))
  let right5 = add(multiply(0, 100), multiply(0, 200))
  assert_eq(left5, right5)
}

test "divide_with_ceil_mathematical_properties" {
  // 测试向上取整除法的数学性质
  // 正数测试
  let ceil_result1 = divide_with_ceil(10, 3)
  assert_eq(4, ceil_result1)  // ceil(3.33) = 4
  
  let ceil_result2 = divide_with_ceil(11, 3)
  assert_eq(4, ceil_result2)  // ceil(3.67) = 4
  
  let ceil_result3 = divide_with_ceil(12, 3)
  assert_eq(4, ceil_result3)  // ceil(4.0) = 4
  
  // 小数测试
  let ceil_result4 = divide_with_ceil(1, 2)
  assert_eq(1, ceil_result4)  // ceil(0.5) = 1
  
  let ceil_result5 = divide_with_ceil(5, 2)
  assert_eq(3, ceil_result5)  // ceil(2.5) = 3
  
  // 负数测试
  let ceil_result6 = divide_with_ceil(-5, 2)
  assert_eq(-2, ceil_result6)  // ceil(-2.5) = -2
  
  let ceil_result7 = divide_with_ceil(-6, 2)
  assert_eq(-3, ceil_result7)  // ceil(-3.0) = -3
  
  let ceil_result8 = divide_with_ceil(-7, 2)
  assert_eq(-3, ceil_result8)  // ceil(-3.5) = -3
}

test "greet_function_empty_and_whitespace_handling" {
  // 测试 greet 函数对空字符串和空白字符的处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t!", greet("\t"))
  assert_eq_string("Hello, \n!", greet("\n"))
  assert_eq_string("Hello, \r!", greet("\r"))
}

test "mathematical_identities_validation" {
  // 验证数学恒等式
  let test_value = 42
  
  // 加法恒等式
  assert_eq(test_value, add(test_value, 0))
  assert_eq(test_value, add(0, test_value))
  
  // 乘法恒等式
  assert_eq(test_value, multiply(test_value, 1))
  assert_eq(test_value, multiply(1, test_value))
  
  // 乘法零元
  assert_eq(0, multiply(test_value, 0))
  assert_eq(0, multiply(0, test_value))
  
  // 加法逆元
  assert_eq(0, add(test_value, -test_value))
}

test "edge_case_combinations" {
  // 边界情况组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(max_val + min_val, add(max_val, min_val))
  
  // 最大值与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(max_val, 0))
  
  // 最小值与0的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(min_val, 0))
}

test "complex_expression_evaluation" {
  // 复杂表达式求值测试
  let a = 10
  let b = 5
  let c = 3
  
  // 表达式1: (a + b) × c
  let expr1 = multiply(add(a, b), c)
  assert_eq(45, expr1)
  
  // 表达式2: a × (b + c)
  let expr2 = multiply(a, add(b, c))
  assert_eq(80, expr2)
  
  // 表达式3: ceil((a × b + c) / 2)
  let expr3 = divide_with_ceil(add(multiply(a, b), c), 2)
  assert_eq(27, expr3)
  
  // 表达式4: ceil(a / b) + ceil(b / c)
  let expr4 = add(divide_with_ceil(a, b), divide_with_ceil(b, c))
  assert_eq(3, expr4)
}