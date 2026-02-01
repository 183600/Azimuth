// 新的标准 MoonBit 测试用例 - 针对 azimuth 库核心功能的测试
// 使用标准的 MoonBit 测试语法编写

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
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

test "subtract_function_basic_operations" {
  // 测试减法基本运算
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(0, -20))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  assert_eq(7, divide_with_ceil(40, 6))  // 40/6 = 6.666... -> 7
  assert_eq(3, divide_with_ceil(-8, 3))  // -8/3 = -2.666... -> -2
  assert_eq(-2, divide_with_ceil(8, -3)) // 8/-3 = -2.666... -> -2
  assert_eq(3, divide_with_ceil(-8, -3)) // -8/-3 = 2.666... -> 3
}

test "greet_function_with_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, JavaScript!", greet("JavaScript"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "combined_operations_practical_scenario" {
  // 测试组合运算的实际场景：计算购物车总价
  let item1_price = 35
  let item1_quantity = 3
  let item2_price = 28
  let item2_quantity = 2
  let discount = 15
  
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let total_before_discount = add(subtotal1, subtotal2)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(105, subtotal1)  // 35 * 3 = 105
  assert_eq(56, subtotal2)   // 28 * 2 = 56
  assert_eq(161, total_before_discount)  // 105 + 56 = 161
  assert_eq(146, final_total)  // 161 - 15 = 146
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算：等差数列求和
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(78, sum)  // 6/2 * (6 + 20) = 3 * 26 = 78
}

test "time_management_calculation" {
  // 测试时间管理计算：项目任务分配
  let total_tasks = 87
  let tasks_per_day = 8
  let working_days = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(working_days, 8)
  
  // 计算剩余时间
  let unused_capacity = multiply(working_days, tasks_per_day) - total_tasks
  
  assert_eq(11, working_days)  // ceil(87/8) = 11
  assert_eq(88, total_hours)   // 11 * 8 = 88
  assert_eq(1, unused_capacity) // 11*8 - 87 = 1
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 1000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  assert_eq(1050, year1_amount)  // 1000 + 50 = 1050
  assert_eq(1103, year2_amount)  // 1050 + 53 = 1103
  assert_eq(1158, year3_amount)  // 1103 + 55 = 1158
}

test "error_handling_boundary_values" {
  // 测试错误处理和边界值
  // 测试零值运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(-9876, 0))
  assert_eq(12345, add(12345, 0))
  assert_eq(-9876, add(-9876, 0))
  
  // 测试除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}