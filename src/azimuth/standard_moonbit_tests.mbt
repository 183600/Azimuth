// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的全面测试

test "add_function_boundary_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(0, add(2147483647, -2147483647))
}

test "multiply_function_identity_elements" {
  // 测试乘法函数的单位元
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
  assert_eq(1, multiply(1, 1))
}

test "subtract_function_various_cases" {
  // 测试减法函数的各种情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(100, 100))
  assert_eq(200, subtract(100, -100))
  assert_eq(-200, subtract(-100, 100))
}

test "divide_with_ceil_precision_cases" {
  // 测试向上取整除法的精度情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(50, 0))
}

test "greet_function_international_support" {
  // 测试问候函数的国际化和特殊字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping = 12
  let discount = 25
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(add(subtotal, tax_amount), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(397, total_before_discount)
  assert_eq(372, final_total)
}

test "mathematical_properties_verification" {
  // 测试数学性质的验证
  let a = 15
  let b = 25
  let c = 10
  
  // 加法交换律和结合律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "resource_planning_scenario" {
  // 测试资源规划场景
  let total_tasks = 127
  let tasks_per_day = 8
  let work_days_per_week = 5
  let team_members = 3
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  let total_person_days = multiply(days_needed, team_members)
  
  assert_eq(16, days_needed)
  assert_eq(4, weeks_needed)
  assert_eq(48, total_person_days)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  // 测试除零处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, multiply(0, -2147483648))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "string_processing_combinations" {
  // 测试字符串处理组合
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  
  let full_name = first_name + " " + last_name
  let name_with_title = full_name + " - " + title
  let greeting = greet(name_with_title)
  
  assert_eq_string("Hello, 张 三 - 工程师!", greeting)
  assert_eq_string("Hello, ABC123!", greet("ABC123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}