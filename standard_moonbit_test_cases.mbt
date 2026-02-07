// 标准 MoonBit 测试用例
// 为 Azimuth 项目添加的高质量测试用例

test "azimuth_mathematical_commutativity" {
  // 测试数学交换律性质
  let a = 17
  let b = 23
  
  // 加法交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(40, add(17, 23))
  
  // 乘法交换律验证
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(391, multiply(17, 23))
}

test "azimuth_division_precision" {
  // 测试除法精度和边界情况
  // 正数除法测试
  assert_eq(4, divide_with_ceil(13, 4))
  assert_eq(7, divide_with_ceil(43, 6))
  assert_eq(1, divide_with_ceil(7, 7))
  
  // 除零安全测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
}

test "azimuth_string_processing" {
  // 测试字符串处理功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "azimuth_negative_arithmetic" {
  // 测试负数算术运算
  // 负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-8, 13))
  assert_eq(-15, add(-20, 5))
  
  // 负数乘法
  assert_eq(21, multiply(-3, -7))
  assert_eq(-24, multiply(-3, 8))
  assert_eq(0, multiply(-5, 0))
}

test "azimuth_business_calculation" {
  // 测试业务计算场景
  // 场景：计算项目资源分配
  let team_members = 9
  let tasks_per_member = 7
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)  // 9 * 7 = 63
  assert_eq(4, sprints_needed)  // ceil(63/20) = 4
}

test "azimuth_complex_workflow" {
  // 测试复杂工作流程
  // 场景：库存管理计算
  let initial_stock = 150
  let new_shipment = 85
  let total_items = add(initial_stock, new_shipment)
  
  let package_size = 25
  let packages_needed = divide_with_ceil(total_items, package_size)
  
  let total_capacity = multiply(packages_needed, package_size)
  let remaining_space = subtract(total_capacity, total_items)
  
  assert_eq(235, total_items)  // 150 + 85 = 235
  assert_eq(10, packages_needed)  // ceil(235/25) = 10
  assert_eq(250, total_capacity)  // 10 * 25 = 250
  assert_eq(15, remaining_space)  // 250 - 235 = 15
}

test "azimuth_edge_case_handling" {
  // 测试边缘情况处理
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(999999, 1000000))
  
  // 零值运算
  assert_eq(0, multiply(0, 1000000))
  assert_eq(1000000, add(1000000, 0))
}

test "azimuth_sequential_operations" {
  // 测试连续操作序列
  let start_value = 10
  
  // 操作序列：((start + 5) * 2) / 3 + 1
  let step1 = add(start_value, 5)        // 15
  let step2 = multiply(step1, 2)         // 30
  let step3 = divide_with_ceil(step2, 3) // 10
  let step4 = add(step3, 1)              // 11
  
  assert_eq(15, step1)
  assert_eq(30, step2)
  assert_eq(10, step3)
  assert_eq(11, step4)
}