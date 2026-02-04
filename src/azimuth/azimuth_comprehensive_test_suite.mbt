// Azimuth 综合测试套件
// 包含针对 azimuth 库核心功能的高质量测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "greeting_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "edge_case_divisions" {
  // 测试除法的边界情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_business_scenario" {
  // 复杂业务场景：计算订单总价
  let product_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  
  assert_eq(30, total_capacity)
  assert_eq(5, remaining_space)
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(500000, subtract(1000000, 500000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = divide_with_ceil(add(multiply(10, 3), 5), 7)
  assert_eq(5, result3)  // ceil((10*3 + 5)/7) = ceil(35/7) = 5
}