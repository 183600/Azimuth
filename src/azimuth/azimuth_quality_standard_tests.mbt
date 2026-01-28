// Azimuth 库的标准测试用例集合
// 包含10个高质量测试用例，覆盖核心功能

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(59, add(a, b))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3)) // 3.666... -> 4
  assert_eq(0, divide_with_ceil(0, 7))  // 0/7 = 0 -> 0
  assert_eq(0, divide_with_ceil(10, 0)) // 除数为0的安全处理
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串测试
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 4
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(371, total_before_discount)
  assert_eq(351, final_total)
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列 1+3+5+...+19
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(100, sum)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "large_number_boundary_testing" {
  // 大数边界测试
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "resource_allocation_scenario" {
  // 资源分配场景：计算项目所需资源
  let total_tasks = 87
  let tasks_per_person = 8
  
  let people_needed = divide_with_ceil(total_tasks, tasks_per_person)
  let regular_hours_per_person = multiply(people_needed - 1, tasks_per_person)
  let overtime_tasks = add(total_tasks, -regular_hours_per_person)
  
  assert_eq(11, people_needed)
  assert_eq(80, regular_hours_per_person)
  assert_eq(7, overtime_tasks)
}

test "string_processing_combination" {
  // 字符串处理组合测试
  let prefix = "Dr."
  let first_name = "Jane"
  let last_name = "Smith"
  
  let full_name = prefix + " " + first_name + " " + last_name
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, Dr. Jane Smith!", formal_greeting)
  
  // 测试特殊字符组合
  let tech_name = "AI_2024@Tech"
  let tech_greeting = greet(tech_name)
  assert_eq_string("Hello, AI_2024@Tech!", tech_greeting)
}