// 标准综合测试用例
// 测试 add 函数的各种边界情况
test "add_overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 测试负数溢出保护
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(min_val, add(-1000000000, -1500000000))
}

// 测试 multiply 函数的溢出保护
test "multiply_overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数乘法溢出
  assert_eq(max_val, multiply(50000, 50000))
  assert_eq(max_val, multiply(max_val, 2))
  
  // 测试负数乘法溢出
  assert_eq(min_val, multiply(-50000, 50000))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试最小值乘法
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

// 测试 greet 函数的各种输入
test "greet_comprehensive" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 特殊字符@#$!", greet("特殊字符@#$"))
  assert_eq_string("Hello,   !", greet("   "))
}

// 测试 divide_with_ceil 函数的各种情况
test "divide_with_ceil_comprehensive" {
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 3))
  assert_eq(0, divide_with_ceil(0, 3))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
  assert_eq(0, divide_with_ceil(-1, 3))
  
  // 负除数
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(-2, divide_with_ceil(6, -3))
  
  // 双负数
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(3, divide_with_ceil(-9, -3))
}

// 测试复合运算
test "complex_operations" {
  let x = 15
  let y = -3
  let z = 7
  
  // 复合加法和乘法
  assert_eq(105, multiply(x, z))
  assert_eq(12, add(x, y))
  assert_eq(-21, multiply(y, z))
  assert_eq(22, add(x, add(y, z)))
  
  // 使用除法函数的复合运算
  assert_eq(5, divide_with_ceil(multiply(x, z), y))
  assert_eq(8, divide_with_ceil(add(x, z), y))
}

// 测试断言函数
test "assertion_functions" {
  // 测试 assert_true 和 assert_false
  assert_true(1 > 0)
  assert_true(0 == 0)
  assert_true(-5 < 0)
  assert_false(1 < 0)
  assert_false(0 != 0)
  assert_false(-5 > 0)
  
  // 测试 assert_eq
  assert_eq(42, 42)
  assert_eq(0, 0)
  assert_eq(-42, -42)
  
  // 测试 assert_eq_string
  assert_eq_string("", "")
  assert_eq_string("a", "a")
  assert_eq_string("Hello, World!", "Hello, World!")
}

// 测试边界值组合
test "edge_case_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法组合
  assert_eq(0, add(max_val, add(min_val, 1)))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法组合
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(-1, max_val))
  
  // 边界值与普通值组合
  assert_eq(max_val, add(max_val - 1, 1))
  assert_eq(min_val, add(min_val + 1, -1))
}

// 测试字符串处理
test "string_processing_comprehensive" {
  // 测试不同长度的字符串
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, abc!", greet("abc"))
  assert_eq_string("Hello, very_long_string!", greet("very_long_string"))
  
  // 测试特殊字符
  assert_eq_string("Hello, \n!", greet("\n"))
  assert_eq_string("Hello, \t!", greet("\t"))
  assert_eq_string("Hello, \"!", greet("\""))
}

// 测试数值运算的一致性
test "arithmetic_consistency" {
  // 测试加法交换律
  let a = 123
  let b = 456
  assert_eq(add(a, b), add(b, a))
  
  // 测试乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试加法结合律
  let c = 789
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 测试乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 测试分配律
  assert_eq(add(multiply(a, c), multiply(b, c)), multiply(add(a, b), c))
}

// 测试错误处理和边界情况
test "error_handling_and_boundaries" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值处理
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(0, multiply(0, 0))
  
  // 测试单位值处理
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(-max_val, multiply(-1, max_val))
  
  // 测试加法中的零值
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, add(0, 0))
}