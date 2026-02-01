// 高质量标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_comprehensive" {
  // 测试加法功能的全面情况
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_edge_cases" {
  // 测试乘法功能的边界情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_basic" {
  // 测试减法功能的基本情况
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(100, subtract(150, 50))
  assert_eq(-100, subtract(50, 150))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的全面情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "packaging_and_resource_calculation" {
  // 测试包装和资源计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let total_weight = multiply(total_items, 2)  // 每个物品2kg
  let weight_per_box_limit = 30  // 每箱限重30kg
  let boxes_by_weight = divide_with_ceil(total_weight, weight_per_box_limit)
  
  // 最终需要的箱数应该是两个约束中的较大值
  let final_boxes = if boxes_needed > boxes_by_weight {
    boxes_needed
  } else {
    boxes_by_weight
  }
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  assert_eq(94, total_weight)  // 47 * 2 = 94
  assert_eq(4, boxes_by_weight)  // ceil(94/30) = 4
  assert_eq(4, final_boxes)
}