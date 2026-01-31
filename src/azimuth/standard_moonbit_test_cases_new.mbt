// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能测试
// 使用标准的 MoonBit 测试语法

test "basic_addition_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(25, multiply(5, 5))
}

test "basic_subtraction_functionality" {
  // 测试基本的减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(2, 7))
  assert_eq(10, subtract(15, 5))
}

test "greet_function_basic_test" {
  // 测试问候函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_properties_zero" {
  // 测试零元素性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 10))
  assert_eq(5, add(5, 0))
  assert_eq(10, add(0, 10))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 计算订单总价：商品价格 × 数量 + 运费
  let product_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "packaging_calculation_scenario" {
  // 测试包装计算场景
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