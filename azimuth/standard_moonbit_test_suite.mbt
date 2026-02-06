// 标准 MoonBit 测试套件 - 为 Azimuth 项目添加
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_basic_addition_comprehensive" {
  // 综合加法测试 - 包含正数、负数和零
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "azimuth_multiplication_edge_cases" {
  // 乘法边界情况测试
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "azimuth_divide_with_ceil_complex_scenarios" {
  // 向上取整除法复杂场景测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "azimuth_greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, éspanol!", greet("éspanol"))
}

test "azimuth_mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律测试
  assert_eq(
    multiply(a, add(b, 3)),
    add(multiply(a, b), multiply(a, 3))
  )
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "azimuth_business_packaging_logic" {
  // 业务包装逻辑测试
  let total_products = 47
  let package_capacity = 10
  let packages_needed = divide_with_ceil(total_products, package_capacity)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, package_capacity)
  assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(3, remaining_space)
}

test "azimuth_subtraction_comprehensive" {
  // 综合减法测试
  assert_eq(7, subtract(10, 3))
  assert_eq(-7, subtract(3, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
  assert_eq(100, subtract(50, -50))
}

test "azimuth_complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源管理和预算计算
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 5
  let sprint_capacity = multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(10, sprint_capacity)  // 2 * 5 = 10
  assert_eq(5, sprints_needed)  // ceil(48/10) = 5
}

test "azimuth_boundary_and_extreme_values" {
  // 边界和极值测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(0, -100))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1000001, divide_with_ceil(1000001, 1))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@#$%!", greet("special@#$%"))
}

test "azimuth_error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算测试
  let large_positive = 1000000
  let large_negative = -1000000
  let zero = 0
  
  assert_eq(large_positive, add(large_positive, zero))
  assert_eq(large_negative, add(large_negative, zero))
  assert_eq(zero, multiply(large_positive, zero))
  assert_eq(zero, multiply(large_negative, zero))
  assert_eq(zero, multiply(zero, large_positive))
  
  // 字符串边界测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, x!", greet("x"))
  assert_eq_string("Hello, XYZ!", greet("XYZ"))
}