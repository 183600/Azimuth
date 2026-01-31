// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "addition_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))  // 15 + 27 = 27 + 15
}

test "multiplication_zero_property" {
  // 测试乘法零性质
  let x = 42
  assert_eq(0, multiply(x, 0))  // 任何数乘以0等于0
  assert_eq(0, multiply(0, x))  // 0乘以任何数等于0
}

test "divide_with_ceil_positive_fractions" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))    // 10/3 = 3.33... -> 4
  assert_eq(3, divide_with_ceil(9, 3))     // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(5, 8))     // 5/8 = 0.625 -> 1
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.33... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // 10/-3 = -3.33... -> -3
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.66... -> 4
}

test "divide_with_ceil_zero_divisor" {
  // 测试除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))   // 除数为0时返回0
  assert_eq(0, divide_with_ceil(-50, 0))   // 负数除以0返回0
  assert_eq(0, divide_with_ceil(0, 0))     // 0除以0返回0
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))   // 空字符串测试
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)           // 25 * 8 = 200
  assert_eq(215, total_before_discount)  // 200 + 15 = 215
  assert_eq(195, final_total)        // 215 - 20 = 195
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}