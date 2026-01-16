// 用户自定义的 MoonBit 测试用例
// 包含不超过10个标准测试用例，使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-3, add(-8, 5))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "add_with_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_with_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "string_processing_with_special_chars" {
  // 测试包含特殊字符的字符串处理
  let special_name = "User@123"
  let result = greet(special_name)
  assert_eq_string("Hello, User@123!", result)
  
  let unicode_name = "测试"
  let unicode_result = greet(unicode_name)
  assert_eq_string("Hello, 测试!", unicode_result)
}

test "assert_functions_validation" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_application" {
  // 测试实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 2
  let shipping = 15
  let discount = 20
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  // 计算折扣后的价格
  let discounted_price = add(subtotal, -discount)
  // 计算最终总价（含运费）
  let total_price = add(discounted_price, shipping)
  
  assert_eq(403, total_price)  // (199×2) - 20 + 15 = 398 - 20 + 15 = 403
}