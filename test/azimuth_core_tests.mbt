// Azimuth 核心功能测试 - 10个标准 MoonBit 测试用例

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_function_standard" {
  // 测试标准问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_business_calculation" {
  // 测试复杂业务计算
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

test "inventory_management" {
  // 测试库存管理场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "string_processing_with_unicode" {
  // 测试Unicode字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(500, result1)
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(170, result2)
}