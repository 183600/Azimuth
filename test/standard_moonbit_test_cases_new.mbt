// 标准 MoonBit 测试用例 - 新增10个测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
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
  assert_eq(100, left_side)  // 5 * (8 + 12) = 5 * 20 = 100
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法功能
  // 正数测试
  assert_eq(4, divide_with_ceil(10, 3))   // 10/3 = 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // 10/-3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.333... -> 4
  
  // 边界测试
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0时返回0
  assert_eq(0, divide_with_ceil(0, 5))     // 0/5 = 0 -> 0
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8  // 8%
  let shipping_cost = 15
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(add(subtotal, tax_amount), shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(534, subtotal)
  assert_eq(43, tax_amount)
  assert_eq(592, total_before_discount)
  assert_eq(542, final_total)
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 8
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(128, sum_formula)  // 8/2 * (6 + 28) = 4 * 34 = 136
}

test "inventory_management_calculation" {
  // 库存管理计算：计算需要的仓储空间
  let product_a_qty = 150
  let product_b_qty = 200
  let product_c_qty = 175
  let items_per_container = 50
  
  // 计算总数量
  let total_items = add(add(product_a_qty, product_b_qty), product_c_qty)
  // 计算需要的容器数量
  let containers_needed = divide_with_ceil(total_items, items_per_container)
  
  assert_eq(525, total_items)
  assert_eq(11, containers_needed)  // ceil(525/50) = 11
}

test "time_and_project_management" {
  // 时间和项目管理：计算项目完成时间
  let total_tasks = 87
  let tasks_per_day = 6
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  assert_eq(15, work_days_needed)  // ceil(87/6) = 15
  assert_eq(3, weeks_needed)       // ceil(15/5) = 3
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  
  // 构建完整姓名
  let full_name = last_name + first_name
  // 构建完整称谓
  let full_title = full_name + title
  // 生成问候
  let greeting = greet(full_title)
  
  assert_eq_string("Hello, 张三工程师!", greeting)
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 工程师!", greet("工程师"))
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 测试极大值
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 测试极小值
  let small_num = 1
  assert_eq(2, add(small_num, small_num))
  assert_eq(1, multiply(small_num, small_num))
  
  // 测试负数运算
  assert_eq(0, add(5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-2, divide_with_ceil(-5, 2))
  
  // 测试零运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(12345, 0))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
}