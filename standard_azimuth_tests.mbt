// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 使用标准的 MoonBit 测试语法编写

// 导入 azimuth 库
use azimuth

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, azimuth::add(2, 3))
  assert_eq(0, azimuth::add(0, 0))
  assert_eq(-5, azimuth::add(-2, -3))
  assert_eq(1000, azimuth::add(500, 500))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(0, azimuth::multiply(0, 5))
  assert_eq(-6, azimuth::multiply(-2, 3))
  assert_eq(100, azimuth::multiply(10, 10))
}

test "greet_function_standard_cases" {
  // 测试标准的问候功能
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 用户!", azimuth::greet("用户"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, azimuth::divide_with_ceil(10, 3))
  assert_eq(3, azimuth::divide_with_ceil(9, 3))
  assert_eq(1, azimuth::divide_with_ceil(1, 100))
  assert_eq(2, azimuth::divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  assert_eq(4, azimuth::divide_with_ceil(-10, -3))
  assert_eq(-2, azimuth::divide_with_ceil(-4, 2))
  assert_eq(0, azimuth::divide_with_ceil(5, 0))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
}

test "zero_element_properties" {
  // 测试零元素性质
  let x = 42
  
  // 加法零元素
  assert_eq(x, azimuth::add(x, 0))
  assert_eq(x, azimuth::add(0, x))
  
  // 乘法零元素
  assert_eq(0, azimuth::multiply(x, 0))
  assert_eq(0, azimuth::multiply(0, x))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 25
  let quantity = 8
  let shipping_cost = 15
  
  let subtotal = azimuth::multiply(unit_price, quantity)
  let total = azimuth::add(subtotal, shipping_cost)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "packaging_calculation_scenario" {
  // 测试包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符处理
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  assert_eq_string("Hello, αβγ!", azimuth::greet("αβγ"))
  assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
}