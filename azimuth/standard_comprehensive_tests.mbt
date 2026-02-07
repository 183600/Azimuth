// 标准 MoonBit 综合测试用例
// 为 Azimuth 项目添加的10个高质量测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(0, 0))
  
  assert_eq(42, multiply(6, 7))
  assert_eq(-24, multiply(-3, 8))
  assert_eq(0, multiply(5, 0))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(8, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(-3, divide_with_ceil(7, -3))
  assert_eq(3, divide_with_ceil(-7, -3))
  assert_eq(25, multiply(-5, -5))
}

test "string_greeting_variations" {
  // 字符串问候变化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 减法性质
  assert_eq(0, subtract(a, a))
  assert_eq(a, subtract(a, 0))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源分配
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
}

test "boundary_values_and_limits" {
  // 边界值和限制测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
}

test "sequential_operations_chain" {
  // 连续操作链测试
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = add(step3, 1)          // 5
  
  assert_eq(5, step4)
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let products_per_box = 24
  let total_products = 185
  let boxes_needed = divide_with_ceil(total_products, products_per_box)
  
  assert_eq(8, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, products_per_box)
  assert_eq(192, total_capacity)
  
  // 验证利用率
  let utilization_percentage = divide_with_ceil(
    multiply(total_products, 100), 
    total_capacity
  )
  assert_eq(97, utilization_percentage)  // floor(18500/192) = 96%
}