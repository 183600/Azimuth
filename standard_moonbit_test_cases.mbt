// 标准 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

test "add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(-5, add(-2, -3))
}

test "add_boundary_values" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(0, min_val))
}

test "multiply_basic_cases" {
  // 基本乘法测试
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
}

test "multiply_special_values" {
  // 特殊值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(min_val, multiply(-1, min_val))  // 特殊情况：最小值乘以-1
}

test "greet_function_test" {
  // greet 函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic" {
  // 向上取整除法基本测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
}

test "divide_with_ceil_negative_divisor" {
  // 负除数的向上取整除法
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(2, divide_with_ceil(-6, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
}

test "combined_operations" {
  // 组合运算测试
  assert_eq(25, add(multiply(3, 5), multiply(2, 5)))
  assert_eq(20, multiply(add(3, 2), 4))
  assert_eq(8, divide_with_ceil(add(multiply(2, 3), 2), 2))
  assert_eq(15, add(multiply(divide_with_ceil(10, 2), 2), 5))
}

test "assert_functions_test" {
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq_string("test", "test")
  assert_eq_string("", "")
}