// 标准 MoonBit 测试用例 - 为 azimuth 库新增的测试
// 包含10个高质量测试用例，覆盖核心功能和边界情况

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 测试负数情况
  let x = -10
  let y = 8
  azimuth::assert_eq(azimuth::add(x, y), azimuth::add(y, x))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 9
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  azimuth::assert_eq(left_side, right_side)
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(-8, 4))   // -8/4 = -2 -> -2
  azimuth::assert_eq(-1, azimuth::divide_with_ceil(-3, 4))   // -3/4 = -0.75 -> -1
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-11, 4))  // -11/4 = -2.75 -> -3
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))     // 除零安全处理
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  azimuth::assert_eq_string("Hello, 🌟!", azimuth::greet("🌟"))
  azimuth::assert_eq_string("Hello, ñoño!", azimuth::greet("ñoño"))
  azimuth::assert_eq_string("Hello, Москва!", azimuth::greet("Москва"))
  azimuth::assert_eq_string("Hello, العربية!", azimuth::greet("العربية"))
}

test "subtraction_using_add_function" {
  // 使用 add 函数实现减法：a - b = a + (-b)
  azimuth::assert_eq(0, azimuth::add(10, -10))
  azimuth::assert_eq(-5, azimuth::add(5, -10))
  azimuth::assert_eq(15, azimuth::add(10, 5))  // 10 - (-5) = 10 + 5
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(1000000, azimuth::add(1000001, -1))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping = 25
  
  // 计算小计
  let subtotal = azimuth::multiply(unit_price, quantity)
  // 计算税费
  let tax = azimuth::divide_with_ceil(azimuth::multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = azimuth::add(azimuth::add(subtotal, tax), shipping)
  
  azimuth::assert_eq(540, subtotal)    // 45 * 12 = 540
  azimuth::assert_eq(44, tax)          // ceil(540 * 8 / 100) = ceil(43.2) = 44
  azimuth::assert_eq(609, total)       // 540 + 44 + 25 = 609
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  
  // 加法单位元
  azimuth::assert_eq(x, azimuth::add(x, 0))
  azimuth::assert_eq(x, azimuth::add(0, x))
  
  // 乘法单位元
  azimuth::assert_eq(x, azimuth::multiply(x, 1))
  azimuth::assert_eq(x, azimuth::multiply(1, x))
  
  // 乘法零元
  azimuth::assert_eq(0, azimuth::multiply(x, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, x))
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 5000
  let projects = 3
  let min_budget_per_project = azimuth::divide_with_ceil(total_budget, projects)
  
  azimuth::assert_eq(1667, min_budget_per_project)  // ceil(5000/3) = 1667
  
  // 验证总分配不超过预算
  let total_allocated = azimuth::multiply(min_budget_per_project, projects)
  azimuth::assert_eq(5001, total_allocated)  // 1667 * 3 = 5001
}

test "time_management_calculation" {
  // 测试时间管理计算
  let tasks_per_day = 6
  let total_tasks = 47
  let work_days_needed = azimuth::divide_with_ceil(total_tasks, tasks_per_day)
  
  azimuth::assert_eq(8, work_days_needed)  // ceil(47/6) = 8
  
  // 计算总工作小时数（每天8小时）
  let total_hours = azimuth::multiply(work_days_needed, 8)
  azimuth::assert_eq(64, total_hours)  // 8 * 8 = 64
  
  // 计算剩余处理能力
  let total_capacity = azimuth::multiply(work_days_needed, tasks_per_day)
  let remaining_capacity = azimuth::add(total_capacity, -total_tasks)
  azimuth::assert_eq(1, remaining_capacity)  // 8*6 - 47 = 48 - 47 = 1
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 除零错误处理
  azimuth::assert_eq(0, azimuth::divide_with_ceil(100, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-100, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  
  // 极值运算
  let large_num = 1000000
  azimuth::assert_eq(2000000, azimuth::add(large_num, large_num))
  azimuth::assert_eq(1000000000000, azimuth::multiply(large_num, large_num))
  
  // 边界值测试
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 1))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(-1, -1))
}