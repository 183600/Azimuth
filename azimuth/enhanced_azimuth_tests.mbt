// 增强的 Azimuth 测试用例 - 使用标准 MoonBit 测试语法

test "add_function_commutativity" {
  // 测试加法交换律
  assert_eq(add(15, 27), add(27, 15))
  assert_eq(add(-8, 12), add(12, -8))
  assert_eq(add(-5, -10), add(-10, -5))
}

test "add_function_associativity" {
  // 测试加法结合律
  assert_eq(add(add(2, 3), 4), add(2, add(3, 4)))
  assert_eq(add(add(-2, 3), 5), add(-2, add(3, 5)))
}

test "multiply_function_distributive" {
  // 测试乘法分配律
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
  assert_eq(multiply(3, add(5, 7)), add(multiply(3, 5), multiply(3, 7)))
}

test "divide_with_ceil_precision" {
  // 测试除法向上取整的精确性
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(4, divide_with_ceil(15, 4))
}

test "divide_with_ceil_negative_precision" {
  // 测试负数除法向上取整
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(-15, 4))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  assert_eq(42, add(42, 0))  // 加法单位元
  assert_eq(42, multiply(42, 1))  // 乘法单位元
  assert_eq(0, multiply(42, 0))  // 乘法零元
}

test "complex_business_logic" {
  // 复杂业务逻辑测试：计算订单总箱数
  let order_quantities = [12, 8, 15, 7, 23]
  let box_capacity = 10
  let total_boxes = 0
  
  // 计算每种商品需要的箱数
  total_boxes = add(total_boxes, divide_with_ceil(order_quantities[0], box_capacity))
  total_boxes = add(total_boxes, divide_with_ceil(order_quantities[1], box_capacity))
  total_boxes = add(total_boxes, divide_with_ceil(order_quantities[2], box_capacity))
  total_boxes = add(total_boxes, divide_with_ceil(order_quantities[3], box_capacity))
  total_boxes = add(total_boxes, divide_with_ceil(order_quantities[4], box_capacity))
  
  assert_eq(8, total_boxes)  // 2+1+2+1+2 = 8
}

test "greet_function_multilingual" {
  // 多语言问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
}

test "edge_case_zero_division" {
  // 除零边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
}

test "performance_large_numbers" {
  // 大数性能测试
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  assert_eq(2, divide_with_ceil(add(large_num, 1), large_num))
}