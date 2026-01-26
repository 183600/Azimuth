// Azimuth 高质量测试套件 - 标准MoonBit测试语法
// 包含10个精心设计的测试用例，覆盖各种场景和边界情况

test "arithmetics_add_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(7, 8))
  assert_eq(15, add(8, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
  assert_eq(1000001, add(1000000, 1))
}

test "arithmetics_multiply_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 3
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)  // 6 * (4 + 3) = 6 * 4 + 6 * 3 = 42
}

test "divide_with_ceil_complex_business_logic" {
  // 复杂业务逻辑：项目资源分配计算
  let total_tasks = 47
  let team_members = 5
  let tasks_per_member_per_day = 2
  let working_days = 10
  
  // 计算每个成员的总任务数
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  assert_eq(10, tasks_per_member)  // ceil(47/5) = 10
  
  // 计算需要的总天数
  let daily_capacity = multiply(team_members, tasks_per_member_per_day)
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  assert_eq(5, days_needed)  // ceil(47/10) = 5
  
  // 验证时间充足性
  assert_true(days_needed <= working_days)
}

test "string_greeting_internationalization" {
  // 测试国际化字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀 MoonBit!", greet("🚀 MoonBit"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "mathematical_identity_elements" {
  // 测试数学单位元性质
  let x = 42
  
  // 加法单位元：a + 0 = a
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元：a * 1 = a
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元：a * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "error_handling_division_by_zero" {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 12
  
  // 计算步骤：((base * 3) + 8) / 5 + 2
  let step1 = multiply(base_value, 3)        // 12 * 3 = 36
  let step2 = add(step1, 8)                  // 36 + 8 = 44
  let step3 = divide_with_ceil(step2, 5)     // ceil(44/5) = 9
  let result = add(step3, 2)                 // 9 + 2 = 11
  
  assert_eq(11, result)
}

test "resource_optimization_problem" {
  // 资源优化问题：仓库存储优化
  // 物品A：37个，每箱容量8
  // 物品B：63个，每箱容量12
  // 物品C：95个，每箱容量15
  
  let item_a = 37
  let item_b = 63
  let item_c = 95
  
  let boxes_a = divide_with_ceil(item_a, 8)   // ceil(37/8) = 5
  let boxes_b = divide_with_ceil(item_b, 12)  // ceil(63/12) = 6
  let boxes_c = divide_with_ceil(item_c, 15)  // ceil(95/15) = 7
  
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(18, total_boxes)
  
  // 计算总存储容量
  let total_capacity = add(
    multiply(boxes_a, 8),
    add(multiply(boxes_b, 12), multiply(boxes_c, 15))
  )
  assert_eq(211, total_capacity)
  
  // 计算利用率
  let total_items = add(add(item_a, item_b), item_c)
  let utilization = multiply(divide_with_ceil(multiply(total_items, 100), total_capacity), 1)
  assert_eq(94, utilization)  // floor(19500/211) = 92%
}

test "edge_cases_boundary_values" {
  // 边界值和极值测试
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  
  // 极小值运算
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 边界情况
  assert_eq(1, divide_with_ceil(999999, 999999))
  assert_eq(2, divide_with_ceil(1000000, 999999))
}

test "boolean_logic_conditions" {
  // 布尔逻辑条件测试
  let positive = 10
  let negative = -5
  let zero = 0
  
  // 正数条件测试
  assert_true(positive > 0)
  assert_false(positive <= 0)
  
  // 负数条件测试
  assert_true(negative < 0)
  assert_false(negative >= 0)
  
  // 零值条件测试
  assert_true(zero == 0)
  assert_false(zero != 0)
  
  // 复合条件测试
  assert_true(add(positive, negative) > 0)  // 10 + (-5) = 5 > 0
  assert_false(multiply(negative, positive) > 0)  // -5 * 10 = -50 < 0
}