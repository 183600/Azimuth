// 专注的 Azimuth 测试用例
// 测试核心功能的边界情况和实际应用场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, add(10, -10))
  assert_eq(100, add(50, 50))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(25, multiply(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_special_cases" {
  // 测试问候函数的特殊情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let product_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(195, final_total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(5, days_needed)
}

test "financial_planning_scenario" {
  // 财务规划场景
  let monthly_income = 5000
  let monthly_expenses = 3200
  let savings_per_month = add(monthly_income, -monthly_expenses)
  let annual_savings = multiply(savings_per_month, 12)
  
  assert_eq(1800, savings_per_month)
  assert_eq(21600, annual_savings)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
}