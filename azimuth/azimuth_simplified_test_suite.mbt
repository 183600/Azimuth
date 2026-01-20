// Azimuth 简化测试用例集
// 包含基本的测试功能

test "basic_addition" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
}

test "basic_multiplication" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(4, -3))
  assert_eq(16, multiply(-4, -4))
}

test "greet_functionality" {
  // 问候函数测试
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 向上取整除法基本测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(5, 3))
}

test "assertion_helpers" {
  // 断言辅助函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("test", "test")
}

test "combined_math_operations" {
  // 复合数学运算测试
  let x = 12
  let y = 8
  
  // (x + y) * 2
  let sum = add(x, y)
  let result = multiply(sum, 2)
  assert_eq(40, result)
  
  // x * y + 10
  let product = multiply(x, y)
  let result2 = add(product, 10)
  assert_eq(106, result2)
}

test "boundary_value_tests" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界加法
  assert_eq(max_val, add(max_val - 1000, 1000))
  assert_eq(min_val, add(min_val + 1000, -1000))
  
  // 边界乘法
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "string_edge_cases" {
  // 字符串边界情况测试
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, C++!", greet("C++"))
}

test "mathematical_properties" {
  // 数学性质验证
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(a, add(a, 0))
  assert_eq(b, add(0, b))
  
  // 乘法单位元
  assert_eq(a, multiply(a, 1))
  assert_eq(b, multiply(1, b))
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let large_num = 1000000
  
  // 大数运算
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000, multiply(large_num, 1))
  assert_eq(0, multiply(large_num, 0))
  
  // 向上取整除法边界
  assert_eq(1000000, divide_with_ceil(large_num, 1))
  assert_eq(1, divide_with_ceil(large_num, large_num))
}