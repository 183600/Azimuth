// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 5
  let b = 3
  let c = 2
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 验证具体数值: 5 * (3 + 2) = 5 * 3 + 5 * 2 = 25
  assert_eq(25, left_side)
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(4, divide_with_ceil(11, 3)) // 3.666... -> 4
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  assert_eq(100, divide_with_ceil(999, 10)) // 99.9 -> 100
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算订单总价（商品价格 × 数量 + 运费 - 折扣）
  let unit_price = 25
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)           // 25 * 8 = 200
  assert_eq(215, total_before_discount) // 200 + 15 = 215
  assert_eq(195, final_total)        // 215 - 20 = 195
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  // 场景：将任务分配给团队成员
  let total_tasks = 47
  let team_members = 5
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  assert_eq(10, tasks_per_member)  // ceil(47/5) = 10
  
  // 验证总分配能力
  let total_capacity = multiply(tasks_per_member, team_members)
  assert_eq(50, total_capacity)
  
  // 验证剩余能力
  let unused_capacity = subtract(total_capacity, total_tasks)
  assert_eq(3, unused_capacity)
}

test "mathematical_identity_elements" {
  // 测试数学单位元
  let x = 42
  
  // 加法单位元：x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元：x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元：x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(-2, add(5, -7))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(2, divide_with_ceil(7, -3))
  assert_eq(3, divide_with_ceil(-7, -3))
}

test "edge_case_boundary_values" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "real_world_packaging_problem" {
  // 真实世界包装问题测试
  // 场景：将物品装入箱子
  let items_count = 73
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items_count, box_capacity)
  
  assert_eq(7, boxes_needed)  // ceil(73/12) = 7
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(84, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, items_count)
  assert_eq(11, remaining_space)
  
  // 验证箱子容量足够
  assert_true(total_capacity >= items_count)
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}