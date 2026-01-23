// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_basic" {
  // 基本加法测试
  assert_eq(7, add(3, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_function_basic" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(7, 0))
  assert_eq(-12, multiply(-3, 4))
}

test "divide_with_ceil_basic" {
  // 基本向上取整除法测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_basic" {
  // 基本问候函数测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "combined_operations" {
  // 组合运算测试
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "negative_operations" {
  // 负数运算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "zero_operations" {
  // 零值运算测试
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "large_numbers" {
  // 大数运算测试
  assert_eq(2000, add(1000, 1000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "precision_tests" {
  // 精度测试
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(11, 3))
}

test "real_world_scenario" {
  // 实际应用场景测试
  let price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(100, subtotal)
  assert_eq(110, total)
}