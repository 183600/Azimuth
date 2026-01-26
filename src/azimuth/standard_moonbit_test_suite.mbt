// 标准 MoonBit 测试套件
// 针对 azimuth 库核心功能的标准测试用例

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(100, multiply(10, 10))
}

test "greeting_functionality" {
  // 问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "ceil_division_positive" {
  // 正数向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "ceil_division_negative" {
  // 负数向上取整除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "mathematical_commutative_property" {
  // 数学交换律测试
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  let unit_price = 25
  let quantity = 4
  let tax_rate = 8
  let shipping = 10
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(100, subtotal)
  assert_eq(8, tax)
  assert_eq(118, total)
}

test "packaging_efficiency" {
  // 包装效率计算测试
  let total_products = 47
  let capacity_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_products, capacity_per_box)
  let unused_capacity = multiply(boxes_needed, capacity_per_box) - total_products
  
  assert_eq(4, boxes_needed)
  assert_eq(1, unused_capacity)
}

test "unicode_string_handling" {
  // Unicode 字符串处理测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
}

test "zero_handling_operations" {
  // 零值处理操作测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, 0))
}