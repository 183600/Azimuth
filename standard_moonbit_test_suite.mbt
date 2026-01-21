// 标准 MoonBit 测试套件
// 包含10个核心测试用例，使用标准MoonBit测试语法

test "add_function_basic_operations" {
  // 测试add函数的基本运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "add_function_boundary_values" {
  // 测试add函数的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, add(min_val, 0))
}

test "multiply_function_basic_operations" {
  // 测试multiply函数的基本运算
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
  assert_eq(25, multiply(5, 5))
}

test "multiply_function_special_cases" {
  // 测试multiply函数的特殊情况
  let min_val = -2147483648
  
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_function_standard_cases" {
  // 测试greet函数的标准情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, User123!", greet("User123"))
}

test "greet_function_unicode_support" {
  // 测试greet函数的Unicode支持
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 矩形面积计算
  let length = 8
  let width = 6
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 周长计算
  let perimeter = multiply(2, add(length, width))
  assert_eq(28, perimeter)
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_business_calculations" {
  // 测试实际业务计算
  // 购物车计算
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(413, total)
  
  // 折扣计算
  let discount = 10
  let discounted_price = multiply(item_price, add(100, -discount)) / 100
  assert_eq(179, discounted_price)
}

test "edge_cases_and_error_handling" {
  // 测试边界情况和错误处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护测试
  assert_true(add(max_val, 100) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
}