// 标准 MoonBit 测试用例 - 8个高质量测试用例
// 覆盖基本功能、边界条件和实际应用场景

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(42, add(15, 27))
  assert_eq(405, multiply(15, 27))
}

test "divide_with_ceil_boundary_conditions" {
  // 测试向上取整除法的边界条件
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 100))
  
  // 测试除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景：项目资源规划
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(40, total_tasks)  // 8 * 5 = 40
  assert_eq(3, sprints_needed)  // ceil(40/15) = 3
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(45, total_capacity)  // 3 * 15 = 45
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(-2, add(5, -7))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(2, divide_with_ceil(-7, -4))
  assert_eq(-1, divide_with_ceil(7, -8))
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
  
  // 验证具体计算
  assert_eq(42, multiply(6, add(4, 3)))
  assert_eq(42, add(multiply(6, 4), multiply(6, 3)))
}

test "real_world_inventory_management" {
  // 真实世界库存管理场景
  let product_a_qty = 47
  let product_b_qty = 32
  let product_c_qty = 89
  
  let box_capacity = 12
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  assert_eq(4, boxes_a)  // ceil(47/12) = 4
  assert_eq(3, boxes_b)  // ceil(32/12) = 3
  assert_eq(8, boxes_c)  // ceil(89/12) = 8
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(15, total_boxes)
  assert_eq(180, total_capacity)
}

test "unicode_and_internationalization" {
  // Unicode和国际字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, éspañol!", greet("éspañol"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}