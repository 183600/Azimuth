// Azimuth 项目标准测试用例
// 测试核心功能：add, multiply, greet 函数以及断言函数

test "add_function_basic_operations" {
  // 测试基本加法运算
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
}

test "add_function_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_operations" {
  // 测试基本乘法运算
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(8, 0))
}

test "multiply_function_special_cases" {
  // 测试乘法特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(46341, 46341))
}

test "greet_function_standard_cases" {
  // 测试问候函数标准情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, \"Test\"!", greet("\"Test\""))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 3
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "assert_functions_verification" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "real_world_application_test" {
  // 实际应用测试：购物车计算
  let item_price = 199
  let quantity = 3
  let discount = 50
  let shipping = 15
  
  // 商品总价
  let subtotal = multiply(item_price, quantity)
  // 折扣后价格
  let discounted = add(subtotal, -discount)
  // 最终价格（含运费）
  let total = add(discounted, shipping)
  
  assert_eq(602, total)  // 199*3 - 50 + 15 = 597 - 50 + 15 = 562
}