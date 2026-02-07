// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(7, add(3, 4))
  assert_eq(12, multiply(3, 4))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(2, subtract(10, 8))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "negative_numbers_handling" {
  // 负数处理测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "string_operations_and_greetings" {
  // 字符串操作和问候测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  // 场景：计算项目资源需求
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "boolean_logic_conditions" {
  // 布尔逻辑条件测试
  assert_true(5 > 3)
  assert_true(0 == 0)
  assert_false(5 < 3)
  assert_false(-1 > 0)
  
  // 测试计算结果的布尔条件
  let result = add(3, 4)
  assert_true(result == 7)
  assert_false(result != 7)
}

test "packaging_and_distribution" {
  // 包装和分发场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(500000, subtract(1000000, 500000))
}

test "sequential_calculation_workflow" {
  // 连续计算工作流测试
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 2)     // 2
  let final_result = add(step4, 3)   // 5
  
  assert_eq(5, final_result)
  
  // 验证中间步骤
  assert_eq(15, step1)
  assert_eq(30, step2)
  assert_eq(4, step3)
  assert_eq(2, step4)
}