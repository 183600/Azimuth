// Quality focused test file - 高质量聚焦测试用例
// 包含 8 个针对 azimuth 库核心功能的测试用例

test "mathematical_commutativity" {
  // 测试数学交换律
  assert_eq(add(15, 25), add(25, 15))
  assert_eq(multiply(7, 9), multiply(9, 7))
}

test "divide_with_ceil_precision" {
  // 测试除法向上取整的精度
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, Name With Spaces!", greet("Name With Spaces"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base_price = 50
  let quantity = 8
  let tax_rate = 10
  let shipping = 15
  
  let subtotal = multiply(base_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(455, total)
}

test "resource_allocation_calculation" {
  // 资源分配计算测试
  let total_resources = 100
  let project1_needs = 23
  let project2_needs = 31
  let project3_needs = 19
  
  let allocated = add(add(project1_needs, project2_needs), project3_needs)
  let remaining = add(total_resources, -allocated)
  let additional_projects = divide_with_ceil(remaining, 10)
  
  assert_eq(73, allocated)
  assert_eq(27, remaining)
  assert_eq(3, additional_projects)
}

test "negative_number_comprehensive" {
  // 负数综合测试
  assert_eq(-10, add(-3, -7))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
}

test "boundary_value_operations" {
  // 边界值操作测试
  assert_eq(1, divide_with_ceil(1, 1000))
  assert_eq(1000, divide_with_ceil(999999, 1000))
  assert_eq(0, divide_with_ceil(0, 1000))
  assert_eq(0, multiply(0, -1000))
  assert_eq(1000, add(1000, 0))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let box_capacity = 24
  let product_a_qty = 35
  let product_b_qty = 67
  let product_c_qty = 12
  
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(2, boxes_a)
  assert_eq(3, boxes_b)
  assert_eq(1, boxes_c)
  assert_eq(6, total_boxes)
}