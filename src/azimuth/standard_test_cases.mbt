// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法编写的高质量测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(3, divide_with_ceil(12, 4))
}

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

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数情况
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  
  // 负数情况
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
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

test "mathematical_boundary_conditions" {
  // 数学边界条件测试
  // 零元素性质
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(0, 5))
  
  // 单位元素性质
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(5, -1))
  assert_eq(-5, multiply(-1, 5))
}

test "edge_cases_and_error_handling" {
  // 边界情况和错误处理测试
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  
  // 极值除法
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1000001, divide_with_ceil(1000001, 1))
  
  // 特殊字符串处理
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
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