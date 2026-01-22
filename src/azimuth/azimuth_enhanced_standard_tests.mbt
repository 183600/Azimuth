// Enhanced Standard MoonBit Tests for Azimuth Library
// 高质量标准测试用例，覆盖核心功能和实际应用场景

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))  // 加法交换律
  assert_eq(multiply(a, b), multiply(b, a))  // 乘法交换律
}

test "divide_with_ceil_precision_handling" {
  // 测试向上取整除法的精度处理
  assert_eq(4, divide_with_ceil(11, 3))   // 11/3 = 3.666... -> 4
  assert_eq(7, divide_with_ceil(20, 3))   // 20/3 = 6.666... -> 7
  assert_eq(-3, divide_with_ceil(-8, 3))  // -8/3 = -2.666... -> -2
  assert_eq(0, divide_with_ceil(0, 5))    // 0 除以任何数
  assert_eq(0, divide_with_ceil(10, 0))   // 除数为零的安全处理
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8  // 8%
  let shipping = 12
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)  // 45 * 6
  assert_eq(22, tax)        // ceil(270 * 8 / 100) = ceil(21.6) = 22
  assert_eq(304, total)     // 270 + 22 + 12
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, !", greet(""))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 22
  
  // 分配资源
  let allocated_a = multiply(project_a_needs, 1)
  let allocated_b = multiply(project_b_needs, 1)
  let allocated_c = multiply(project_c_needs, 1)
  
  // 计算总分配和剩余资源
  let total_allocated = add(add(allocated_a, allocated_b), allocated_c)
  let remaining = add(total_resources, -total_allocated)
  
  // 计算可以支持的小项目数量
  let small_projects = divide_with_ceil(remaining, 8)
  
  assert_eq(85, total_allocated)  // 35 + 28 + 22
  assert_eq(15, remaining)        // 100 - 85
  assert_eq(2, small_projects)    // ceil(15 / 8) = 2
}

test "time_management_calculations" {
  // 时间管理计算测试
  let daily_work_hours = 8
  let total_tasks = 55
  let tasks_per_hour = 3
  
  // 计算需要的工作天数
  let total_hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let work_days_needed = divide_with_ceil(total_hours_needed, daily_work_hours)
  
  // 计算实际工作时间
  let actual_work_hours = multiply(work_days_needed, daily_work_hours)
  
  assert_eq(19, total_hours_needed)  // ceil(55 / 3) = 19
  assert_eq(3, work_days_needed)     // ceil(19 / 8) = 3
  assert_eq(24, actual_work_hours)   // 3 * 8
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1050, year1)  // 1000 + ceil(1000 * 5 / 100) = 1000 + 50
  assert_eq(1103, year2)  // 1050 + ceil(1050 * 5 / 100) = 1050 + 53
  assert_eq(1158, year3)  // 1103 + ceil(1103 * 5 / 100) = 1103 + 55
}

test "edge_cases_boundary_values" {
  // 边界值和极端情况测试
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-4, divide_with_ceil(-11, 3))
  
  // 测试零值运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, add(0, 0))
  assert_eq(0, divide_with_ceil(0, 100))
}