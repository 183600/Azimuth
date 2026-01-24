// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "add_function_commutativity_extended" {
  // 扩展测试加法交换律，包含更多边界情况
  assert_eq(100, add(99, 1))
  assert_eq(100, add(1, 99))
  assert_eq(-100, add(-99, -1))
  assert_eq(-100, add(-1, -99))
  assert_eq(0, add(100, -100))
  assert_eq(0, add(-100, 100))
}

test "multiply_function_power_calculation" {
  // 测试乘法函数的幂运算场景
  assert_eq(1, multiply(1, 1))
  assert_eq(4, multiply(2, 2))
  assert_eq(9, multiply(3, 3))
  assert_eq(16, multiply(4, 4))
  assert_eq(25, multiply(5, 5))
  assert_eq(36, multiply(6, 6))
}

test "greet_function_format_validation" {
  // 测试问候函数的格式验证
  assert_eq_string("Hello, John_Doe!", greet("John_Doe"))
  assert_eq_string("Hello, user@domain!", greet("user@domain"))
  assert_eq_string("Hello, 123-456!", greet("123-456"))
  assert_eq_string("Hello, A.B.C!", greet("A.B.C"))
}

test "divide_with_ceil_complex_fractions" {
  // 测试向上取整除法的复杂分数情况
  assert_eq(2, divide_with_ceil(5, 3))  // 1.666... -> 2
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(4, divide_with_ceil(11, 3)) // 3.666... -> 4
  assert_eq(5, divide_with_ceil(14, 3)) // 4.666... -> 5
  assert_eq(6, divide_with_ceil(17, 3)) // 5.666... -> 6
}

test "nested_function_composition" {
  // 测试嵌套函数组合
  let base = 10
  let result = divide_with_ceil(
    add(
      multiply(base, 3),
      multiply(base, 2)
    ),
    add(base, divide_with_ceil(base, 3))
  )
  // 手动计算: (10*3 + 10*2) / (10 + ceil(10/3)) = 50 / 14 = ceil(3.57) = 4
  assert_eq(4, result)
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算
  let first_term = 1
  let second_term = 1
  let third_term = add(first_term, second_term)  // 斐波那契数列
  let fourth_term = add(second_term, third_term)
  let fifth_term = add(third_term, fourth_term)
  
  assert_eq(2, third_term)
  assert_eq(3, fourth_term)
  assert_eq(5, fifth_term)
}

test "business_invoice_calculation" {
  // 测试商业发票计算
  let unit_price = 45
  let quantity = 7
  let tax_rate = 8
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(315, subtotal)
  assert_eq(26, tax_amount)  // ceil(315 * 8 / 100) = ceil(25.2) = 26
  assert_eq(341, total_before_discount)
  assert_eq(321, final_total)
}

test "time_conversion_scenarios" {
  // 测试时间转换场景
  let hours = 5
  let total_hours = hours  // 定义total_hours变量
  let minutes_per_hour = 60
  let total_minutes = multiply(hours, minutes_per_hour)
  
  let seconds_per_minute = 60
  let total_seconds = multiply(total_minutes, seconds_per_minute)
  
  let days = divide_with_ceil(total_hours, 24)
  
  assert_eq(300, total_minutes)
  assert_eq(18000, total_seconds)
  assert_eq(1, days)  // ceil(5/24) = 1
}

test "string_pattern_validation" {
  // 测试字符串模式验证
  assert_eq_string("Hello, UPPER!", greet("UPPER"))
  assert_eq_string("Hello, lower!", greet("lower"))
  assert_eq_string("Hello, MixedCase!", greet("MixedCase"))
  assert_eq_string("Hello, camelCase!", greet("camelCase"))
  assert_eq_string("Hello, snake_case!", greet("snake_case"))
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 22
  
  let allocated_a = project_a_needs
  let allocated_b = project_b_needs
  let allocated_c = project_c_needs
  let total_allocated = add(add(allocated_a, allocated_b), allocated_c)
  let remaining = add(total_resources, -total_allocated)
  
  // 计算可以支持的小项目数量
  let small_project_size = 5
  let additional_projects = divide_with_ceil(remaining, small_project_size)
  
  assert_eq(85, total_allocated)
  assert_eq(15, remaining)
  assert_eq(3, additional_projects)  // ceil(15/5) = 3
}