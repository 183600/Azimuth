// Azimuth 简单测试用例
// 包含9个标准 MoonBit 测试用例

test "simple_add" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "simple_multiply" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_divide_ceil" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "simple_divide_ceil_negative" {
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "simple_greet" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_subtract" {
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "simple_commutative" {
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "simple_packaging" {
  let items = 25
  let capacity = 6
  let packages = divide_with_ceil(items, capacity)
  assert_eq(5, packages)
  
  let total_capacity = multiply(packages, capacity)
  let remaining = subtract(total_capacity, items)
  assert_eq(5, remaining)
}

test "simple_workflow" {
  let team = 4
  let tasks_per_person = 8
  let total_tasks = multiply(team, tasks_per_person)
  let sprint_capacity = 15
  let sprints = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(32, total_tasks)  // 4 * 8 = 32
  assert_eq(3, sprints)       // ceil(32/15) = 3
}

// 新增的测试用例
test "mathematical_identities" {
  // 数学恒等式测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 最小边界值
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 最大边界值
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  
  // 特殊边界值
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  let employees = 15
  let projects_per_employee = 4
  let total_projects = multiply(employees, projects_per_employee)
  
  let sprints_per_project = 3
  let total_sprints = multiply(total_projects, sprints_per_project)
  
  let sprints_per_quarter = 20
  let quarters_needed = divide_with_ceil(total_sprints, sprints_per_quarter)
  
  assert_eq(60, total_projects)     // 15 * 4 = 60
  assert_eq(180, total_sprints)     // 60 * 3 = 180
  assert_eq(9, quarters_needed)     // ceil(180/20) = 9
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零情况的各种测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let product_types = 8
  let items_per_type = 125
  let total_items = multiply(product_types, items_per_type)
  
  let box_capacity = 50
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  let cost_per_box = 25
  let total_cost = multiply(boxes_needed, cost_per_box)
  
  assert_eq(1000, total_items)      // 8 * 125 = 1000
  assert_eq(20, boxes_needed)       // ceil(1000/50) = 20
  assert_eq(500, total_cost)        // 20 * 25 = 500
}