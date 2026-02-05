// Azimuth 项目增强 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_comprehensive_test" {
  // 综合加法功能测试
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(2, add(-8, 10))
  assert_eq(5000, add(2500, 2500))
}

test "multiply_function_advanced_test" {
  // 高级乘法功能测试
  assert_eq(30, multiply(5, 6))
  assert_eq(0, multiply(12, 0))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(18, multiply(-3, -6))
  assert_eq(144, multiply(12, 12))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  assert_eq(5, divide_with_ceil(14, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(15, 0))
}

test "divide_with_ceil_negative_comprehensive" {
  // 向上取整除法负数综合测试
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(-5, divide_with_ceil(14, -3))
  assert_eq(5, divide_with_ceil(-14, -3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "greet_function_international_test" {
  // 问候函数国际化测试
  assert_eq_string("Hello, Universe!", greet("Universe"))
  assert_eq_string("Hello, 开发者!", greet("开发者"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, 456!", greet("456"))
  assert_eq_string("Hello, emoji 🚀!", greet("emoji 🚀"))
}

test "subtract_function_comprehensive_test" {
  // 综合减法功能测试
  assert_eq(7, subtract(15, 8))
  assert_eq(-7, subtract(8, 15))
  assert_eq(0, subtract(20, 20))
  assert_eq(25, subtract(15, -10))
  assert_eq(-25, subtract(-15, 10))
}

test "mathematical_laws_validation" {
  // 数学定律验证测试
  let x = 9
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(20, add(9, 11))
  assert_eq(99, multiply(9, 11))
}

test "logistics_distribution_scenario" {
  // 物流分配场景测试
  let total_packages = 58
  let trucks_available = 7
  let packages_per_truck = divide_with_ceil(total_packages, trucks_available)
  
  assert_eq(9, packages_per_truck)
  
  // 验证总运力
  let total_capacity = multiply(packages_per_truck, trucks_available)
  assert_eq(63, total_capacity)
  
  // 验证剩余运力
  let remaining_capacity = subtract(total_capacity, total_packages)
  assert_eq(5, remaining_capacity)
}

test "project_resource_calculation" {
  // 项目资源计算测试
  let developers = 9
  let stories_per_developer = 5
  let total_stories = multiply(developers, stories_per_developer)
  
  let sprint_capacity = 22
  let sprints_needed = divide_with_ceil(total_stories, sprint_capacity)
  
  assert_eq(45, total_stories)  // 9 * 5 = 45
  assert_eq(3, sprints_needed)  // ceil(45/22) = 3
}

test "extreme_boundary_conditions" {
  // 极端边界条件测试
  // 除法边界条件
  assert_eq(1, divide_with_ceil(7, 7))
  assert_eq(1, divide_with_ceil(-8, -8))
  assert_eq(0, divide_with_ceil(0, 8))
  
  // 大数运算
  assert_eq(3000000, add(1500000, 1500000))
  assert_eq(4000000, multiply(2000, 2000))
  
  // 特殊问候情况
  assert_eq_string("Hello,    !", greet("    "))
  assert_eq_string("Hello, symbols#$%!", greet("symbols#$%"))
}