// Azimuth 项目标准测试用例
// 包含8个高质量测试用例，覆盖所有主要功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  add(5, 3) = 8
  add(0, 0) = 0
  add(-5, 3) = -2
  add(-5, -3) = -8
  add(1000, 2000) = 3000
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边缘情况
  multiply(4, 5) = 20
  multiply(0, 123) = 0
  multiply(1, 456) = 456
  multiply(-2, 8) = -16
  multiply(-3, -7) = 21
  multiply(100, 100) = 10000
}

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  subtract(10, 4) = 6
  subtract(4, 10) = -6
  subtract(15, 15) = 0
  subtract(-5, 3) = -8
  subtract(10, -3) = 13
  subtract(-8, -3) = -5
}

test "divide_with_ceil_boundary_conditions" {
  // 测试向上取整除法的边界条件
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(1, 5) = 1
  divide_with_ceil(0, 7) = 0
  divide_with_ceil(15, 0) = 0
  divide_with_ceil(-10, 3) = -3
  divide_with_ceil(10, -3) = -3
  divide_with_ceil(-10, -3) = 4
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  greet("World") = "Hello, World!"
  greet("MoonBit") = "Hello, MoonBit!"
  greet("") = "Hello, !"
  greet("世界") = "Hello, 世界!"
  greet("café") = "Hello, café!"
  greet("🚀") = "Hello, 🚀!"
  greet("123") = "Hello, 123!"
}

test "mathematical_properties_validation" {
  // 验证数学性质
  // 加法交换律
  add(7, 13) = add(13, 7)
  add(-5, 10) = add(10, -5)
  
  // 乘法交换律
  multiply(6, 9) = multiply(9, 6)
  multiply(-4, 7) = multiply(7, -4)
  
  // 加法恒等式
  add(15, 0) = 15
  multiply(15, 1) = 15
  multiply(15, 0) = 0
}

test "business_inventory_management" {
  // 业务场景：库存管理测试
  let total_items = 87
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  // 验证需要的箱子数量
  boxes_needed = 8
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  total_capacity = 96
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  remaining_space = 9
}

test "complex_project_calculation" {
  // 复杂项目计算场景
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  // 验证计算结果
  total_tasks = 48  // 6 * 8 = 48
  sprints_needed = 3  // ceil(48/20) = 3
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  total_capacity = 60  // 3 * 20 = 60
  
  // 验证剩余处理能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  remaining_capacity = 12  // 60 - 48 = 12
}