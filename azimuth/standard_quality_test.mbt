// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition_comprehensive" {
  // 基本加法综合测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(100, add(50, 50))
}

test "multiplication_various_cases" {
  // 乘法函数各种情况测试
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(100, 0))
  assert_eq(-30, multiply(-6, 5))
  assert_eq(30, multiply(-6, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(8, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-2, divide_with_ceil(-8, 4))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
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

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}

// 新增的8个标准 MoonBit 测试用例
test "enhanced_arithmetic_operations" {
  // 增强算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-15, add(-7, -8))
  assert_eq(1, add(-7, 8))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(56, multiply(-7, -8))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(17, subtract(10, -7))
  assert_eq(-17, subtract(-10, 7))
}

test "divide_with_ceil_precision_testing" {
  // 向上取整除法精度测试
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(667, divide_with_ceil(2000, 3))
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
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

test "unicode_and_emoji_greeting" {
  // Unicode和表情符号问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "complex_sequential_operations" {
  // 复杂连续操作测试
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 1)     // 3
  let step5 = multiply(step4, 3)     // 9
  
  assert_eq(9, step5)
}

test "business_inventory_management" {
  // 业务库存管理测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "project_resource_planning" {
  // 项目资源规划测试
  let team_members = 9
  let tasks_per_member = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 22
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(45, total_tasks)  // 9 * 5 = 45
  assert_eq(3, sprints_needed)  // ceil(45/22) = 3
}