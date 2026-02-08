// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "standard_add_function_comprehensive" {
  // 加法函数综合测试
  inspect(azimuth::add(3, 5))  // 期望: 8
  inspect(azimuth::add(0, 0))  // 期望: 0
  inspect(azimuth::add(-5, 3)) // 期望: -2
  inspect(azimuth::add(50, 50)) // 期望: 100
  inspect(azimuth::add(-50, -50)) // 期望: -100
}

test "standard_multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  inspect(azimuth::multiply(3, 5)) // 期望: 15
  inspect(azimuth::multiply(5, 0)) // 期望: 0
  inspect(azimuth::multiply(-3, 5)) // 期望: -15
  inspect(azimuth::multiply(-3, -5)) // 期望: 15
  inspect(azimuth::multiply(100, 100)) // 期望: 10000
}

test "standard_divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  inspect(azimuth::divide_with_ceil(8, 3)) // 期望: 3
  inspect(azimuth::divide_with_ceil(6, 3)) // 期望: 2
  inspect(azimuth::divide_with_ceil(1, 5)) // 期望: 1
  inspect(azimuth::divide_with_ceil(0, 10)) // 期望: 0
  inspect(azimuth::divide_with_ceil(10, 0)) // 期望: 0
}

test "standard_divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  inspect(azimuth::divide_with_ceil(-8, 3)) // 期望: -2
  inspect(azimuth::divide_with_ceil(8, -3)) // 期望: -3
  inspect(azimuth::divide_with_ceil(-8, -3)) // 期望: 3
  inspect(azimuth::divide_with_ceil(-1, 2)) // 期望: -1
  inspect(azimuth::divide_with_ceil(-1, -2)) // 期望: 1
}

test "standard_greet_function_various_inputs" {
  // 问候函数各种输入测试
  inspect(azimuth::greet("MoonBit")) // 期望: "Hello, MoonBit!"
  inspect(azimuth::greet("World")) // 期望: "Hello, World!"
  inspect(azimuth::greet("")) // 期望: "Hello, !"
  inspect(azimuth::greet("123")) // 期望: "Hello, 123!"
  inspect(azimuth::greet("中文")) // 期望: "Hello, 中文!"
}

test "standard_mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 7
  
  // 加法交换律
  inspect(azimuth::add(a, b)) // 期望: 19
  inspect(azimuth::add(b, a)) // 期望: 19
  
  // 乘法交换律
  inspect(azimuth::multiply(a, b)) // 期望: 84
  inspect(azimuth::multiply(b, a)) // 期望: 84
}

test "standard_business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = azimuth::divide_with_ceil(total_items, items_per_package)
  inspect(packages_needed) // 期望: 5
  
  // 验证总容量
  let total_capacity = azimuth::multiply(packages_needed, items_per_package)
  inspect(total_capacity) // 期望: 40
  
  // 验证剩余空间
  let remaining_space = azimuth::subtract(total_capacity, total_items)
  inspect(remaining_space) // 期望: 5
}

test "standard_complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = azimuth::multiply(team_members, tasks_per_member)
  inspect(total_tasks) // 期望: 48
  
  let sprint_capacity = 15
  let sprints_needed = azimuth::divide_with_ceil(total_tasks, sprint_capacity)
  inspect(sprints_needed) // 期望: 4
}