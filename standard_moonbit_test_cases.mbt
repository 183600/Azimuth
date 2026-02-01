// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(7, 3))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(12, multiply(3, 4))
}

test "subtract_function_basic" {
  // 测试基本减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-3, subtract(-2, 1))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = subtract(multiply(10, 5), add(15, 10))
  assert_eq(25, result2)  // 10*5 - (15 + 10) = 50 - 25 = 25
}

test "boundary_values" {
  // 测试边界值
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "negative_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "large_numbers" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(0, subtract(1000000, 1000000))
}

test "practical_scenario" {
  // 测试实际应用场景：计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = subtract(total_before_discount, discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}