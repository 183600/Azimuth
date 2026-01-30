// 新的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(7, 8))
  assert_eq(15, add(8, 7))
  assert_eq(0, add(5, -5))
  assert_eq(100, add(60, 40))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 5
  let b = 3
  let c = 2
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(10, 0))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_function_multilingual_support" {
  // 测试多语言问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列
  let first_term = 3
  let common_diff = 2
  let num_terms = 5
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(35, sum)  // 3 + 5 + 7 + 9 + 11 = 35
}

test "inventory_packaging_optimization" {
  // 库存包装优化
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, box_capacity)
  let unused_space = add(total_capacity, -total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, unused_space)
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数
  let work_hours_per_day = 8
  let total_work_hours = multiply(days_needed, work_hours_per_day)
  
  assert_eq(5, days_needed)
  assert_eq(40, total_work_hours)
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let interest_rate = 5
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
}

test "string_processing_with_greet" {
  // 字符串处理与问候功能结合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试特殊字符
  let special_name = "用户@123"
  let special_greeting = greet(special_name)
  assert_eq_string("Hello, 用户@123!", special_greeting)
}