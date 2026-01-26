// 标准 Azimuth 测试用例
// 使用标准的 MoonBit 测试语法编写的测试用例集

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的行为
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(3, divide_with_ceil(10, -3))  // 10/-3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3)) // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))    // 0/5 = 0 -> 0
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为0时的安全处理
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景：订单总价计算
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_cost = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(subtotal, add(tax_amount, shipping_cost))
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(400, total_before_discount)
  assert_eq(380, final_total)
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列求和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：S = n/2 × (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(120, sum_formula)  // 10/2 × (6 + 18) = 5 × 24 = 120
}

test "time_and_resource_calculation" {
  // 时间和资源计算：项目规划场景
  let total_tasks = 47
  let tasks_per_day = 8
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(workdays_needed, working_days_per_week)
  
  assert_eq(6, workdays_needed)  // ceil(47/8) = 6
  assert_eq(2, weeks_needed)     // ceil(6/5) = 2
}

test "error_resilience_and_boundary_values" {
  // 错误恢复和边界值测试
  // 测试极大值
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 测试极小值
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(1000000, multiply(-1000, -1000))
  
  // 测试零值运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(12345, 0))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
}

test "string_processing_combinations" {
  // 字符串处理组合测试
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  
  // 构建完整姓名和职位
  let full_name = first_name + last_name
  let full_title = full_name + "：" + title
  let greeting = greet(full_title)
  
  assert_eq_string("Hello, 张三：工程师!", greeting)
  
  // 测试特殊字符组合
  assert_eq_string("Hello, Test@2024!", greet("Test@2024"))
  assert_eq_string("Hello, C++/Java!", greet("C++/Java"))
}