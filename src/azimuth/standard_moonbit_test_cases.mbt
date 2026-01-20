// 标准 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(10, add(-5, 15))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 3))
}

test "greet_function_basic" {
  // 测试 greet 函数基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let length = 8
  let width = 6
  let height = 4
  
  // 计算表面积：2*(长*宽 + 长*高 + 宽*高)
  let side1 = multiply(length, width)
  let side2 = multiply(length, height)
  let side3 = multiply(width, height)
  let surface_area = multiply(2, add(add(side1, side2), side3))
  
  assert_eq(208, surface_area)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_application" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let tax_rate = 8
  let shipping = 15
  
  // 计算商品总价
  let subtotal = add(multiply(item1_price, item1_quantity), multiply(item2_price, item2_quantity))
  // 计算税费
  let tax = multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(931, total)
}

test "assert_functions_validation" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
}