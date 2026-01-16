// 用户添加的额外 MoonBit 测试用例
// 包含对 divide_with_ceil 函数和复杂场景的测试

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(5, divide_with_ceil(21, 5))
}

test "divide_with_ceil_exact_division" {
  // 测试能够整除的情况
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(7, divide_with_ceil(14, 2))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "divide_with_ceil_with_remainder" {
  // 测试有余数的情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(6, divide_with_ceil(25, 5))
  assert_eq(8, divide_with_ceil(15, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(-1, divide_with_ceil(-3, 4))
}

test "complex_calculation_with_divide" {
  // 测试包含除法的复杂计算
  // 计算平均值并向上取整
  let numbers_sum = add(add(add(15, 25), 35), 45)
  let count = 4
  let average_ceil = divide_with_ceil(numbers_sum, count)
  assert_eq(31, average_ceil)
}

test "page_calculation_scenario" {
  // 实际应用场景：分页计算
  let total_items = 95
  let items_per_page = 10
  let needed_pages = divide_with_ceil(total_items, items_per_page)
  assert_eq(10, needed_pages)
  
  // 测试刚好整除的情况
  let exact_items = 100
  let exact_pages = divide_with_ceil(exact_items, items_per_page)
  assert_eq(10, exact_pages)
}

test "resource_allocation" {
  // 资源分配场景测试
  let total_resources = 57
  let group_size = 8
  let needed_groups = divide_with_ceil(total_resources, group_size)
  assert_eq(8, needed_groups)
  
  // 测试资源刚好分配完的情况
  let exact_resources = 48
  let exact_groups = divide_with_ceil(exact_resources, group_size)
  assert_eq(6, exact_groups)
}

test "time_slot_calculation" {
  // 时间槽计算测试
  let total_minutes = 125
  let slot_duration = 30
  let needed_slots = divide_with_ceil(total_minutes, slot_duration)
  assert_eq(5, needed_slots)
  
  // 测试时间刚好用完的情况
  let exact_minutes = 120
  let exact_slots = divide_with_ceil(exact_minutes, slot_duration)
  assert_eq(4, exact_slots)
}

test "mixed_operations_complex" {
  // 混合运算复杂测试
  let base = 100
  let multiplier = 3
  let divisor = 4
  
  // 计算 (base * multiplier) / divisor 的向上取整
  let product = multiply(base, multiplier)
  let result = divide_with_ceil(product, divisor)
  assert_eq(75, result)
  
  // 更复杂的计算：((a + b) * c) / d
  let a = 15
  let b = 25
  let c = 6
  let d = 8
  
  let sum_ab = add(a, b)
  let product_abc = multiply(sum_ab, c)
  let complex_result = divide_with_ceil(product_abc, d)
  assert_eq(31, complex_result)
}