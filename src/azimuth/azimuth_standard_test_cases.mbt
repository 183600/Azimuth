// 标准 MoonBit 测试用例 - 为 azimuth 库添加的核心功能测试
// 包含简洁但全面的测试，覆盖主要功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
}

test "greeting_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "ceil_division_basic" {
  // 测试基本向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "negative_number_handling" {
  // 测试负数处理
  assert_eq(-10, add(-3, -7))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "mathematical_properties" {
  // 测试数学性质
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

test "complex_business_scenario" {
  // 测试复杂业务场景
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

test "packaging_calculation" {
  // 测试包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(1000000, add(500000, 500000))
  assert_eq(10000, multiply(100, 100))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "edge_case_handling" {
  // 测试边界情况处理
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq_string("Hello, A!", greet("A"))
}

// 新增的高质量测试用例
test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源分配计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  let buffer_tasks = 5
  let final_tasks = add(total_tasks, buffer_tasks)
  let final_sprints = divide_with_ceil(final_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
  assert_eq(53, final_tasks)  // 48 + 5 = 53
  assert_eq(4, final_sprints)  // ceil(53/15) = 4
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_distributive_law" {
  // 数学分配律验证测试
  let a = 7
  let b = 5
  let c = 3
  
  // 分配律：a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  assert_eq(56, left_side)  // 7 * (5 + 3) = 7 * 8 = 56
  assert_eq(56, right_side) // 7 * 5 + 7 * 3 = 35 + 21 = 56
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：仓库存储优化
  let item_a = 25  // 物品A数量
  let item_b = 47  // 物品B数量
  let item_c = 156 // 物品C数量
  let capacity_a = 8  // 物品A每箱容量
  let capacity_b = 12 // 物品B每箱容量
  let capacity_c = 20 // 物品C每箱容量
  
  let boxes_a = divide_with_ceil(item_a, capacity_a)  // ceil(25/8) = 4
  let boxes_b = divide_with_ceil(item_b, capacity_b)  // ceil(47/12) = 4
  let boxes_c = divide_with_ceil(item_c, capacity_c)  // ceil(156/20) = 8
  
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(16, total_boxes)
  
  // 计算总存储容量
  let total_capacity = add(
    multiply(boxes_a, capacity_a),
    add(multiply(boxes_b, capacity_b), multiply(boxes_c, capacity_c))
  )
  assert_eq(240, total_capacity)
  
  // 计算总物品数量
  let total_items = add(add(item_a, item_b), item_c)
  assert_eq(228, total_items)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(12, remaining_space)
}