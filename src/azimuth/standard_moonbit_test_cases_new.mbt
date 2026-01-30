// 标准 MoonBit 测试用例 - 新增10个测试
// 测试 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "mathematical_commutative_law" {
  // 验证加法和乘法交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(4, divide_with_ceil(12, 3))  // 12/3 = 4 -> 4
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零的安全处理
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, 世界!", greet("世界"))  // Unicode字符
  assert_eq_string("Hello, 123!", greet("123"))  // 数字字符串
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

test "inventory_management_scenario" {
  // 库存管理场景：计算需要的存储空间
  let items_per_box = 24
  let total_items_small = 100
  let total_items_medium = 250
  
  let boxes_small = divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = divide_with_ceil(total_items_medium, items_per_box)
  let total_boxes = add(boxes_small, boxes_medium)
  
  assert_eq(5, boxes_small)   // ceil(100/24) = 5
  assert_eq(11, boxes_medium) // ceil(250/24) = 11
  assert_eq(16, total_boxes)  // 5 + 11 = 16
}

test "mathematical_series_calculation" {
  // 数学级数计算：计算等差数列的和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
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