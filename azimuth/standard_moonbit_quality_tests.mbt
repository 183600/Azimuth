// 标准 MoonBit 质量测试用例
// 为 Azimuth 项目添加的高质量测试覆盖

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(0, add(0, 0))
  assert_eq(10, add(7, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
  assert_eq(-1, add(-8, 7))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, multiply(1, 1))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtract_function_validation" {
  // 减法函数验证测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_workflow" {
  // 业务包装工作流测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let initial = 15
  let step1 = multiply(initial, 2)      // 30
  let step2 = add(step1, 10)           // 40
  let step3 = divide_with_ceil(step2, 9)  // 5
  let result = subtract(step3, 2)      // 3
  
  assert_eq(3, result)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let team_count = 8
  let resources_per_team = divide_with_ceil(total_resources, team_count)
  
  assert_eq(13, resources_per_team)
  
  // 验证分配的总量
  let allocated_total = multiply(resources_per_team, team_count)
  assert_eq(104, allocated_total)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(96, utilization)
}