// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含 8 个高质量测试用例，覆盖基本功能和边界情况

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(a, b))
  assert_eq(42, add(b, a))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)
}

test "divide_with_ceil_boundary_cases" {
  // 测试向上取整除法的边界情况
  // 完全整除的情况
  assert_eq(7, divide_with_ceil(21, 3))
  assert_eq(15, divide_with_ceil(45, 3))
  
  // 需要向上取整的情况
  assert_eq(8, divide_with_ceil(22, 3))
  assert_eq(16, divide_with_ceil(46, 3))
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 计算订单总价：商品价格 × 数量 + 运费 - 折扣
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(348, final_total)  // 89*4 + 12 - 20 = 356 + 12 - 20 = 348
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(2, add(-8, 10))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "zero_element_properties" {
  // 测试零元素性质
  assert_eq(5, add(5, 0))  // 加法单位元
  assert_eq(0, multiply(5, 0))  // 乘法零元
  assert_eq(0, divide_with_ceil(0, 10))  // 零除以正数
  assert_eq(0, divide_with_ceil(5, 0))  // 除数为零的安全处理
}

test "real_world_inventory_calculation" {
  // 测试实际库存计算场景
  let total_items = 157
  let items_per_box = 24
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 验证计算结果
  assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  
  // 验证容量计算
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(168, total_capacity)  // 7 * 24 = 168
  
  // 验证剩余空间
  let remaining_space = add(total_capacity, -total_items)
  assert_eq(11, remaining_space)  // 168 - 157 = 11
}