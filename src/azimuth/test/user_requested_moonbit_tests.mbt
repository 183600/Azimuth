// 用户请求的 MoonBit 测试用例
// 包含 10 个标准 MoonBit 测试语法编写的测试用例

test "user_add_function_basic" {
  // 测试 add 函数的基本功能
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(0, azimuth::add(0, 0))
  assert_eq(-5, azimuth::add(-2, -3))
}

test "user_add_function_negative" {
  // 测试 add 函数的负数运算
  assert_eq(-10, azimuth::add(-3, -7))
  assert_eq(5, azimuth::add(-10, 15))
  assert_eq(-5, azimuth::add(10, -15))
}

test "user_multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  assert_eq(21, azimuth::multiply(3, 7))
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(-15, azimuth::multiply(3, -5))
}

test "user_multiply_function_large" {
  // 测试 multiply 函数的大数运算
  assert_eq(1000000, azimuth::multiply(1000, 1000))
  assert_eq(-1000000, azimuth::multiply(-1000, 1000))
}

test "user_greet_function_standard" {
  // 测试 greet 函数的标准用法
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "user_greet_function_special_chars" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, user@example.com!", azimuth::greet("user@example.com"))
  assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "user_mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
}

test "user_boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  
  // 边界值乘法
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "user_complex_calculation" {
  // 测试复杂计算场景
  // 计算：(5 + 3) * 4 - 6
  let step1 = azimuth::add(5, 3)  // 8
  let step2 = azimuth::multiply(step1, 4)  // 32
  let result = azimuth::add(step2, -6)  // 26
  
  assert_eq(26, result)
}

test "user_real_world_application" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  // 计算商品总价
  let subtotal = azimuth::multiply(item_price, quantity)
  // 计算最终总价（含运费）
  let total = azimuth::add(subtotal, shipping)
  
  assert_eq(413, total)  // 199*2 + 15 = 398 + 15 = 413
}