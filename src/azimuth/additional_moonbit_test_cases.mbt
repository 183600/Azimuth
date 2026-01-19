// 额外的 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-50, 50))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
}

test "add_large_boundary_values" {
  // 测试大边界值相加
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
}

test "multiply_with_large_numbers" {
  // 测试大数相乘
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(46340, multiply(215, 215))  // 接近 sqrt(max_val)
  assert_eq(-1000000, multiply(1000, -1000))
}

test "greet_with_various_names" {
  // 测试不同类型的名称
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c = (10 + 20) * 30 = 900
  let result1 = multiply(add(a, b), c)
  assert_eq(900, result1)
  
  // a * b + c = 10 * 20 + 30 = 230
  let result2 = add(multiply(a, b), c)
  assert_eq(230, result2)
}

test "edge_case_zero_operations" {
  // 测试零的运算
  assert_eq(0, add(0, 0))
  assert_eq(5, add(0, 5))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
}

test "string_operations_with_special_characters" {
  // 测试包含特殊字符的字符串操作
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  assert_eq_string("Hello, \"test\"!", greet("\"test\""))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法函数
  assert_eq(3, divide_with_ceil(10, 4))  // 10/4 = 2.5 → 3
  assert_eq(3, divide_with_ceil(12, 4))  // 12/4 = 3 → 3
  assert_eq(-2, divide_with_ceil(-10, 4))  // -10/4 = -2.5 → -2
  assert_eq(0, divide_with_ceil(1, 4))  // 1/4 = 0.25 → 1
}

test "assert_functions_various_types" {
  // 测试断言函数的各种类型
  assert_true(5 > 3)
  assert_true(0 == 0)
  assert_true(-10 < 0)
  
  assert_false(5 < 3)
  assert_false(0 != 0)
  assert_false(-10 > 0)
  
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq(0, 0)
  
  assert_eq_string("test", "test")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", "Hello, World!")
}