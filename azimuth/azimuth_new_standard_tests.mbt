// Azimuth 项目新增标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，覆盖核心功能

test "azimuth_add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "azimuth_multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
}

test "azimuth_divide_with_ceil_various_scenarios" {
  // 向上取整除法多种场景测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "azimuth_divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "azimuth_greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "azimuth_subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
}

test "azimuth_mathematical_properties_validation" {
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

test "azimuth_business_packaging_scenario" {
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