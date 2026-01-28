// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 使用标准的 MoonBit 测试语法编写

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_properties_identity" {
  // 测试单位元性质
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(-3, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(-10, multiply(-10, 1))
}

test "combined_operations_practical" {
  // 测试组合运算的实际应用
  // 计算购物车总价：商品价格 × 数量 + 运费
  let product_price = 25
  let quantity = 4
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(100, subtotal)
  assert_eq(115, total)
}

test "packaging_calculation_scenario" {
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}