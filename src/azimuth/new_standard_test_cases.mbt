// 标准 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-2, 3))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(min_val, 1))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_special_cases" {
  // 测试乘法的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "assert_functions_verification" {
  // 测试断言函数的正常工作
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试条件断言
  let x = 10
  let y = 20
  assert_true(x < y)
  assert_false(x > y)
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算组合
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(45, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(53, result2)
  
  // (a - b) * (b + c)
  let result3 = multiply(add(a, -b), add(b, c))
  assert_eq(40, result3)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let price = 199
  let quantity = 3
  let shipping = 15
  let total = add(multiply(price, quantity), shipping)
  assert_eq(612, total)
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "edge_cases_and_error_handling" {
  // 测试边界情况和错误处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护验证
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(min_val, 2) == min_val)
  
  // 零值处理
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正常除法（无余数）
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(3, divide_with_ceil(9, 3))
  
  // 向上取整（有余数）
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(8, 3))
  
  // 负数测试
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
}