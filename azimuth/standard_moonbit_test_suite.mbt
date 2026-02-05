// 标准 MoonBit 测试套件 - 为 Azimuth 项目添加
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition_comprehensive" {
  // 综合加法测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
}

test "multiplication_various_scenarios" {
  // 各种乘法场景测试
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 42))
  assert_eq(-24, multiply(-4, 6))
  assert_eq(24, multiply(-4, -6))
  assert_eq(100000, multiply(100, 1000))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(5, divide_with_ceil(17, 4))
  assert_eq(4, divide_with_ceil(16, 4))
  assert_eq(1, divide_with_ceil(3, 10))
  assert_eq(0, divide_with_ceil(0, 8))
  assert_eq(0, divide_with_ceil(25, 0))
}

test "divide_with_ceil_negative_comprehensive" {
  // 向上取整除法负数综合测试
  assert_eq(-4, divide_with_ceil(-17, 4))
  assert_eq(-5, divide_with_ceil(17, -4))
  assert_eq(4, divide_with_ceil(-17, -4))
  assert_eq(-1, divide_with_ceil(-3, 5))
  assert_eq(1, divide_with_ceil(-3, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(8, subtract(15, 7))
  assert_eq(-8, subtract(7, 15))
  assert_eq(0, subtract(25, 25))
  assert_eq(30, subtract(15, -15))
  assert_eq(-30, subtract(-15, 15))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 18
  let y = 7
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(25, add(18, 7))
  assert_eq(126, multiply(18, 7))
}

test "warehouse_packaging_scenario" {
  // 仓库包装场景测试
  let total_products = 89
  let boxes_per_pallet = 12
  let pallets_needed = divide_with_ceil(total_products, boxes_per_pallet)
  
  assert_eq(8, pallets_needed)
  
  // 验证总容量
  let total_capacity = multiply(pallets_needed, boxes_per_pallet)
  assert_eq(96, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(7, remaining_space)
}

test "project_resource_calculation" {
  // 项目资源计算测试
  let developers = 9
  let tasks_per_developer = 7
  let total_tasks = multiply(developers, tasks_per_developer)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)  // 9 * 7 = 63
  assert_eq(4, sprints_needed)  // ceil(63/20) = 4
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(8, 8))
  assert_eq(1, divide_with_ceil(-8, -8))
  assert_eq(0, divide_with_ceil(0, 12))
  
  // 大数运算
  assert_eq(3000000, add(1500000, 1500000))
  assert_eq(25000000, multiply(5000, 5000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
}