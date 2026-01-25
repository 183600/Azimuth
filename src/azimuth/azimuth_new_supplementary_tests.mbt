// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的补充测试

test "simple_addition_test" {
  // 简单的加法测试
  let result = add(10, 20)
  assert_eq(30, result)
}

test "simple_multiplication_test" {
  // 简单的乘法测试
  let result = multiply(5, 6)
  assert_eq(30, result)
}

test "simple_greet_test" {
  // 简单的问候测试
  let result = greet("MoonBit")
  assert_eq_string("Hello, MoonBit!", result)
}

test "simple_divide_with_ceil_test" {
  // 简单的向上取整除法测试
  let result = divide_with_ceil(10, 3)
  assert_eq(4, result)
}

test "combined_operations_test" {
  // 组合运算测试
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "business_calculation_test" {
  // 业务计算测试
  let price = 25
  let quantity = 4
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, 10)  // 加上运费
  assert_eq(100, subtotal)
  assert_eq(110, total)
}

test "packaging_calculation_test" {
  // 包装计算测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)
}

test "edge_case_zero_test" {
  // 零边界情况测试
  let result1 = multiply(5, 0)
  let result2 = add(0, 10)
  let result3 = divide_with_ceil(0, 5)
  assert_eq(0, result1)
  assert_eq(10, result2)
  assert_eq(0, result3)
}

test "negative_number_test" {
  // 负数测试
  let result1 = add(-5, 3)
  let result2 = multiply(-2, 4)
  let result3 = divide_with_ceil(-10, 3)
  assert_eq(-2, result1)
  assert_eq(-8, result2)
  assert_eq(-3, result3)
}

test "large_number_test" {
  // 大数测试
  let result1 = add(10000, 20000)
  let result2 = multiply(100, 200)
  assert_eq(30000, result1)
  assert_eq(20000, result2)
}