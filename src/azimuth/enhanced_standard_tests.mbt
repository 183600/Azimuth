// 增强的标准 MoonBit 测试用例
// 测试 azimuth 包的全面功能

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(-100, add(-50, -50))
}

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(999999, add(500000, 499999))
  assert_eq(-2000000, add(-1000000, -1000000))
}

test "multiply_by_zero" {
  // 测试乘以零的情况
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -10))
  assert_eq(0, multiply(1000000, 0))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(100, multiply(-10, -10))
}

test "divide_with_ceil_negative_dividend" {
  // 测试负被除数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负除数的向上取整除法
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(0, divide_with_ceil(1, -2))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "divide_with_ceil_exact_division" {
  // 测试能够整除的情况
  assert_eq(5, divide_with_ceil(15, 3))
  assert_eq(10, divide_with_ceil(20, 2))
  assert_eq(-5, divide_with_ceil(-15, 3))
  assert_eq(-10, divide_with_ceil(20, -2))
}

test "greet_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABCDEF!", greet("ABCDEF"))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑计算
  // 计算订单总价：商品价格 × 数量 + 运费 - 折扣
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(358, final_total)  // 89*4 + 12 - 20 = 356 + 12 - 20 = 348
}

test "packaging_optimization" {
  // 测试包装优化计算
  // 计算最少需要的包装箱数量
  let items_1 = 47
  let box_capacity_1 = 12
  let boxes_needed_1 = divide_with_ceil(items_1, box_capacity_1)
  
  let items_2 = 100
  let box_capacity_2 = 25
  let boxes_needed_2 = divide_with_ceil(items_2, box_capacity_2)
  
  let total_boxes = add(boxes_needed_1, boxes_needed_2)
  
  assert_eq(4, boxes_needed_1)  // ceil(47/12) = 4
  assert_eq(4, boxes_needed_2)  // ceil(100/25) = 4
  assert_eq(8, total_boxes)     // 4 + 4 = 8
}