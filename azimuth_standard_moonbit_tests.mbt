// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例
// 测试覆盖核心功能：加法、乘法、减法、除法向上取整、问候函数

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(-12345, multiply(12345, -1))
  assert_eq(12345, multiply(-12345, -1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(1000, -1000))
}

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(100, 100))
  assert_eq(200, subtract(100, -100))
  assert_eq(-200, subtract(-100, 100))
  assert_eq(0, subtract(-100, -100))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(34, divide_with_ceil(100, 3))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
  
  // 特殊情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_unicode_and_special_chars" {
  // 测试问候函数的 Unicode 和特殊字符处理
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  // 订单计算：商品价格 × 数量 + 运费 - 折扣
  let unit_price = 89
  let quantity = 4
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(371, total_before_discount)
  assert_eq(351, final_total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景计算
  // 计算需要的包装箱数量
  let small_items = 25
  let medium_items = 47
  let large_items = 83
  let items_per_box = 12
  
  let small_boxes = divide_with_ceil(small_items, items_per_box)
  let medium_boxes = divide_with_ceil(medium_items, items_per_box)
  let large_boxes = divide_with_ceil(large_items, items_per_box)
  let total_boxes = add(add(small_boxes, medium_boxes), large_boxes)
  
  assert_eq(3, small_boxes)   // ceil(25/12) = 3
  assert_eq(4, medium_boxes)  // ceil(47/12) = 4
  assert_eq(7, large_boxes)   // ceil(83/12) = 7
  assert_eq(14, total_boxes)  // 3 + 4 + 7 = 14
}