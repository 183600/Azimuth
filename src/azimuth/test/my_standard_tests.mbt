// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的10个高质量测试用例

test "basic_arithmetic_addition" {
  // 基本算术加法测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "basic_arithmetic_multiplication" {
  // 基本算术乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(5, 5))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_basic" {
  // 基本问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 25
  let quantity = 4
  let shipping = 12
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(100, subtotal)
  assert_eq(112, total)
}

test "packaging_calculation" {
  // 包装计算测试
  let total_items = 23
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(24, total_capacity)
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "combined_operations" {
  // 组合运算测试
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(add(multiply(5, 3), 7), 4)
  assert_eq(6, result2)  // ceil((5*3 + 7)/4) = ceil(22/4) = 6
}