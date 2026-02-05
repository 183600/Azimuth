// 额外的 MoonBit 测试用例 - 针对 azimuth 库的核心功能
test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 4 * (6 + 3) = 4 * 6 + 4 * 3 = 36
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // 10/-3 = -3.333... -> -3
  
  // 负数除负数
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 除数为零
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_with_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
}

test "complex_arithmetic_operations" {
  // 复杂算术运算测试
  let x = 12
  let y = 8
  let z = 5
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(100, result1)  // (12 + 8) * 5 = 20 * 5 = 100
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(101, result2)  // 12 * 8 + 5 = 96 + 5 = 101
  
  // 测试嵌套表达式
  let result3 = add(multiply(divide_with_ceil(x, z), y), multiply(z, 3))
  assert_eq(31, result3)  // ceil(12/5) * 8 + 5 * 3 = 3 * 8 + 15 = 39
}

test "real_world_business_calculations" {
  // 真实业务计算场景
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8
  let shipping = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let total_before_discount = add(add(subtotal, tax), shipping)
  
  // 计算最终总价
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(540, subtotal)
  assert_eq(44, tax)
  assert_eq(609, total_before_discount)
  assert_eq(559, final_total)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(100, 0))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(100, 0))
  
  // 单位运算
  assert_eq(7, multiply(7, 1))
  assert_eq(7, add(7, 0))
  
  // 负数运算
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(3, -7))
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
}

test "string_processing_and_greeting" {
  // 字符串处理和问候测试
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试特殊组合
  let prefix = "Dr."
  let name = "Smith"
  let title = greet(prefix + " " + name)
  assert_eq_string("Hello, Dr. Smith!", title)
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式测试
  // 计算等差数列前n项和: S = n/2 * (2a + (n-1)d)
  let first_term = 3
  let common_diff = 4
  let num_terms = 6
  
  let series_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(78, series_sum)  // 6/2 * (6 + 20) = 3 * 26 = 78
  
  // 验证手动计算
  let manual_sum = add(add(add(add(add(3, 7), 11), 15), 19), 23)
  assert_eq(78, manual_sum)
}

test "resource_allocation_and_planning" {
  // 资源分配和规划测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以启动的小项目数量（每个小项目需要200）
  let small_projects_count = divide_with_ceil(remaining_budget, 200)
  
  assert_eq(3900, allocated_budget)
  assert_eq(1100, remaining_budget)
  assert_eq(6, small_projects_count)
}