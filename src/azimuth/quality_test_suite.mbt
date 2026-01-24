// 高质量 MoonBit 测试套件
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(-24, multiply(-8, 3))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(1, 1))
}

test "divide_with_ceil_advanced" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "edge_cases_and_boundary_values" {
  // 边界值和极端情况测试
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(1, divide_with_ceil(1, 2147483647))
  assert_eq(2147483647, multiply(1, 2147483647))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "real_world_scenarios" {
  // 真实世界应用场景测试
  // 库存管理：计算需要的存储箱数量
  let total_items = 157
  let items_per_box = 24
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(7, boxes_needed)
  
  // 订单计算：商品总价
  let unit_price = 89
  let quantity = 6
  let order_total = multiply(unit_price, quantity)
  assert_eq(534, order_total)
}

test "error_handling_and_safety" {
  // 错误处理和安全性测试
  // 除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算稳定性
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
}

test "string_processing_internationalization" {
  // 字符串处理和国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
}