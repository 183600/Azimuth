// 标准MoonBit测试用例
// 测试azimuth库的核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(7, add(3, 4))
  assert_eq(0, add(5, -5))
  assert_eq(-3, add(-1, -2))
}

test "add_function_boundary" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(5, 0))
}

test "multiply_function_boundary" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_standard" {
  // 测试标准的greet功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
}

test "complex_calculation" {
  // 测试复合计算：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_scenario" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 商品总价 = 单价 × 数量
  let subtotal = multiply(item_price, quantity)
  
  // 最终总价 = 商品总价 + 运费
  let total = add(subtotal, shipping)
  
  assert_eq(612, total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试长字符串
  let long_name = "VeryLongNameWithManyCharactersForTesting"
  assert_eq_string("Hello, VeryLongNameWithManyCharactersForTesting!", greet(long_name))
  
  // 测试包含特殊字符的字符串
  let special_name = "user@domain.com"
  assert_eq_string("Hello, user@domain.com!", greet(special_name))
  
  // 测试包含空格的字符串
  let spaced_name = "John Doe"
  assert_eq_string("Hello, John Doe!", greet(spaced_name))
}

test "assert_functions_verification" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试断言函数与实际函数结合
  let add_result = add(10, 20)
  assert_true(add_result > 0)
  assert_false(add_result < 0)
  
  let multiply_result = multiply(5, 6)
  assert_eq(30, multiply_result)
}