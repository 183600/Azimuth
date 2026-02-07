// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_basic_addition" {
  // 基本加法运算测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-5, 2))
  assert_eq(100, add(50, 50))
}

test "azimuth_basic_multiplication" {
  // 基本乘法运算测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(-5, -5))
}

test "azimuth_divide_with_ceil_basic" {
  // 向上取整除法基本测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "azimuth_divide_with_ceil_negative" {
  // 向上取整除法负数测试
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "azimuth_greet_function" {
  // 问候函数测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "azimuth_subtract_function" {
  // 减法函数测试
  assert_eq(3, subtract(8, 5))
  assert_eq(-3, subtract(5, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(5, -10))
  assert_eq(-15, subtract(-5, 10))
}

test "azimuth_mathematical_properties" {
  // 数学性质验证测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(7, 13))
  assert_eq(91, multiply(7, 13))
}

test "azimuth_business_packaging" {
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

test "azimuth_workflow_calculation" {
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

test "azimuth_boundary_values" {
  // 边界情况和极值测试
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