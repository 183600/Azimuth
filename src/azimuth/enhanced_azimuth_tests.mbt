// Enhanced Azimuth Tests - 高质量测试用例
// 测试 azimuth 库的核心功能和边界情况

test "enhanced_mathematical_operations" {
  // 测试数学运算的组合
  let x = 15
  let y = 7
  let z = 3
  
  // 测试结合律：(x + y) + z = x + (y + z)
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 测试分配律：x * (y + z) = x * y + x * z
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "enhanced_division_ceil_properties" {
  // 测试向上取整除法的数学性质
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(5, divide_with_ceil(21, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "enhanced_string_greeting_scenarios" {
  // 测试各种字符串问候场景
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
}

test "enhanced_business_calculation" {
  // 业务计算：订单总价
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 12
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax)
  assert_eq(304, total)
}

test "enhanced_negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "enhanced_large_number_precision" {
  // 大数精度测试
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(33334, divide_with_ceil(100000, 3))
}

test "enhanced_zero_handling" {
  // 零值处理测试
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(12345, 0))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
}

test "enhanced_complex_expressions" {
  // 复杂表达式测试
  let a = 8
  let b = 12
  let c = 5
  
  // 表达式：(a + b) * c - a
  let result = add(multiply(add(a, b), c), -a)
  assert_eq(92, result)  // (8 + 12) * 5 - 8 = 20 * 5 - 8 = 100 - 8 = 92
  
  // 表达式：a * b + c * (a + b)
  let result2 = add(multiply(a, b), multiply(c, add(a, b)))
  assert_eq(196, result2)  // 8 * 12 + 5 * (8 + 12) = 96 + 5 * 20 = 96 + 100 = 196
}

test "enhanced_resource_allocation" {
  // 资源分配测试
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 22
  
  let allocated = add(add(project_a_needs, project_b_needs), project_c_needs)
  let remaining = add(total_resources, -allocated)
  
  assert_eq(85, allocated)
  assert_eq(15, remaining)
  
  // 计算可以支持的小项目数量
  let small_project_size = 8
  let additional_projects = divide_with_ceil(remaining, small_project_size)
  assert_eq(2, additional_projects)
}

test "enhanced_time_management" {
  // 时间管理测试
  let tasks_per_hour = 4
  let total_tasks = 35
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  
  // 计算总分钟数
  let total_minutes = multiply(hours_needed, 60)
  
  assert_eq(9, hours_needed)  // ceil(35/4) = 9
  assert_eq(540, total_minutes)  // 9 * 60 = 540
}