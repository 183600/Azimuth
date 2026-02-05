// 高质量测试套件 - Azimuth 项目
// 包含 10 个标准 MoonBit 测试用例

test "quality_arithmetic_addition" {
  // 高质量加法算术测试
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-5, 2))
  assert_eq(100, add(45, 55))
  assert_eq(5000, add(2500, 2500))
}

test "quality_arithmetic_multiplication" {
  // 高质量乘法算术测试
  assert_eq(42, multiply(6, 7))
  assert_eq(0, multiply(0, 25))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(18, multiply(-3, -6))
  assert_eq(10000, multiply(100, 100))
}

test "quality_division_ceil_positive" {
  // 高质量向上取整除法正数测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 10))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(8, 0))
}

test "quality_division_ceil_negative" {
  // 高质量向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-11, 3))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "quality_greeting_function" {
  // 高质量问候函数测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
}

test "quality_subtraction_operation" {
  // 高质量减法运算测试
  assert_eq(6, subtract(15, 9))
  assert_eq(-6, subtract(9, 15))
  assert_eq(0, subtract(20, 20))
  assert_eq(25, subtract(15, -10))
  assert_eq(-25, subtract(-15, 10))
}

test "quality_mathematical_laws" {
  // 高质量数学定律测试
  let x = 14
  let y = 9
  
  // 加法交换律验证
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律验证
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 具体数值验证
  assert_eq(23, add(14, 9))
  assert_eq(126, multiply(14, 9))
}

test "quality_inventory_management" {
  // 高质量库存管理测试
  let total_products = 58
  let box_capacity = 15
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(60, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(2, remaining_space)
}

test "quality_project_planning" {
  // 高质量项目规划测试
  // 场景：计算项目资源需求
  let developers = 9
  let tasks_per_developer = 6
  let total_tasks = multiply(developers, tasks_per_developer)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(54, total_tasks)  // 9 * 6 = 54
  assert_eq(3, sprints_needed)  // ceil(54/20) = 3
}

test "quality_boundary_conditions" {
  // 高质量边界条件测试
  // 除法边界测试
  assert_eq(1, divide_with_ceil(7, 7))
  assert_eq(1, divide_with_ceil(-7, -7))
  assert_eq(0, divide_with_ceil(0, 7))
  
  // 大数运算测试
  assert_eq(3000000, add(1500000, 1500000))
  assert_eq(25000000, multiply(5000, 5000))
  
  // 特殊字符串测试
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, &*%$!", greet("&*%$"))
}