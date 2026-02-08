// 标准 MoonBit 测试用例
// 覆盖核心功能和边界情况

test "add_zero_properties" {
  // 测试加法的零性质
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(-42, add(-42, 0))
  assert_eq(-42, add(0, -42))
  assert_eq(0, add(0, 0))
}

test "multiply_identity_properties" {
  // 测试乘法的单位元性质
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(-42, multiply(-42, 1))
  assert_eq(-42, multiply(1, -42))
  assert_eq(0, multiply(0, 1))
  assert_eq(0, multiply(1, 0))
}

test "multiply_negative_properties" {
  // 测试乘法的负数性质
  assert_eq(-42, multiply(42, -1))
  assert_eq(-42, multiply(-1, 42))
  assert_eq(42, multiply(-42, -1))
  assert_eq(42, multiply(-1, -42))
}

test "add_commutativity" {
  // 测试加法交换律
  assert_eq(add(15, 27), add(27, 15))
  assert_eq(add(-15, 27), add(27, -15))
  assert_eq(add(-15, -27), add(-27, -15))
}

test "divide_with_ceil_exact_division" {
  // 测试向上取整除法的精确除法
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(7, divide_with_ceil(21, 3))
  assert_eq(-5, divide_with_ceil(-10, 2))
  assert_eq(-3, divide_with_ceil(-9, 3))
}

test "divide_with_ceil_remainder_cases" {
  // 测试向上取整除法的余数情况
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-5, divide_with_ceil(-9, 2))
  assert_eq(-3, divide_with_ceil(-8, 3))
}

test "greet_empty_and_single_char" {
  // 测试字符串拼接的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello,  !", greet(" "))
}

test "add_overflow_safety" {
  // 测试加法溢出安全性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 这些操作应该安全地返回边界值
  let result1 = add(max_val, 1)
  let result2 = add(max_val, max_val)
  let result3 = add(min_val, -1)
  
  // 验证结果在有效范围内
  assert_true(result1 == max_val || result1 == min_val)
  assert_true(result2 == max_val || result2 == min_val)
  assert_true(result3 == max_val || result3 == min_val)
}

test "multiply_overflow_safety" {
  // 测试乘法溢出安全性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 这些操作应该安全地返回边界值
  let result1 = multiply(max_val, 2)
  let result2 = multiply(max_val, max_val)
  let result3 = multiply(min_val, -1)
  
  // 验证结果在有效范围内
  assert_true(result1 == max_val || result1 == min_val)
  assert_true(result2 == max_val || result2 == min_val)
  assert_true(result3 == max_val || result3 == min_val)
}

test "complex_expressions" {
  // 测试复杂表达式
  let a = 5
  let b = 3
  let c = 2
  
  // 复合运算
  assert_eq(16, multiply(add(a, b), c))
  assert_eq(11, add(multiply(a, b), c))
  assert_eq(4, divide_with_ceil(add(a, b), c))
  assert_eq(9, add(divide_with_ceil(multiply(a, b), c), 5))
}

test "sequential_calculation_test" {
  // 连续计算测试
  let start = 5
  let step1 = add(start, 3)        // 8
  let step2 = multiply(step1, 2)   // 16
  let step3 = subtract(step2, 6)    // 10
  let step4 = divide_with_ceil(step3, 4)  // 3
  
  assert_eq(8, step1)
  assert_eq(16, step2)
  assert_eq(10, step3)
  assert_eq(3, step4)
}

test "azimuth_comprehensive_integration_test" {
  // Azimuth 综合集成测试
  // 测试字符串和数学运算的集成
  let name = "Test"
  let greeting = greet(name)
  assert_eq_string("Hello, Test!", greeting)
  
  // 计算问候语的总字符数
  let hello_len = 5  // "Hello"
  let comma_space_len = 2  // ", "
  let name_len = 4  // "Test"
  let exclamation_len = 1  // "!"
  let total_len = add(hello_len, add(comma_space_len, add(name_len, exclamation_len)))
  assert_eq(12, total_len)
  
  // 使用数学运算验证字符数
  let avg_part_len = divide_with_ceil(total_len, 3)
  assert_eq(4, avg_part_len)
}