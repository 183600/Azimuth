// Azimuth 核心功能测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-5, 2))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(64, multiply(8, 8))
  assert_eq(0, multiply(100, 0))
  assert_eq(-42, multiply(-6, 7))
  assert_eq(42, multiply(-6, -7))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(7, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(-3, divide_with_ceil(10, -4))
  assert_eq(3, divide_with_ceil(-10, -4))
  assert_eq(-1, divide_with_ceil(-1, 3))
  assert_eq(1, divide_with_ceil(-1, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, café!", greet("café"))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(6, subtract(15, 9))
  assert_eq(-6, subtract(9, 15))
  assert_eq(0, subtract(20, 20))
  assert_eq(25, subtract(10, -15))
  assert_eq(-25, subtract(-10, 15))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 18
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(30, add(18, 12))
  assert_eq(216, multiply(18, 12))
}

test "warehouse_packaging_scenario" {
  // 仓库包装场景测试
  let total_products = 89
  let box_capacity = 15
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(6, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(90, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(1, remaining_space)
}

test "project_resource_calculation" {
  // 项目资源计算测试
  let developers = 7
  let tasks_per_developer = 9
  let total_tasks = multiply(developers, tasks_per_developer)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)  // 7 * 9 = 63
  assert_eq(4, sprints_needed)  // ceil(63/20) = 4
}

test "boundary_and_extreme_values" {
  // 边界和极值测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(8, 8))
  assert_eq(1, divide_with_ceil(-8, -8))
  assert_eq(0, divide_with_ceil(0, 8))
  
  // 大数运算
  assert_eq(3000000, add(1500000, 1500000))
  assert_eq(25000000, multiply(5000, 5000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%^&*!", greet("@#$%^&*"))
}