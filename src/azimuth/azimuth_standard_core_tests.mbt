// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含10个基本功能测试和边界情况测试

test "azimuth_add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(45, 55))
}

test "azimuth_multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 7))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(64, multiply(8, 8))
}

test "azimuth_greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "azimuth_divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "azimuth_divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-5, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "azimuth_mathematical_commutative_laws" {
  // 测试数学交换律
  let x = 12
  let y = 8
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "azimuth_combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "azimuth_practical_business_scenario" {
  // 实际业务场景：计算订单总价
  let unit_price = 50
  let quantity = 4
  let shipping = 8
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(208, total)
}

test "azimuth_packaging_calculation" {
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "azimuth_unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}