// 标准 MoonBit 综合测试用例
// 针对 azimuth 库核心功能的高质量测试

test "add_function_core_functionality" {
  // 测试加法核心功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 测试乘法综合功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "subtract_function_basic" {
  // 测试减法基本功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))  // 10/3 = 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.33 -> -3
  assert_eq(0, divide_with_ceil(100, 0))  // 除数为0返回0
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "business_order_calculation" {
  // 业务订单计算场景
  let unit_price = 35
  let quantity = 6
  let shipping_cost = 12
  let discount = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(210, subtotal)
  assert_eq(222, total_before_discount)
  assert_eq(207, final_total)
}

test "packaging_optimization_scenario" {
  // 包装优化场景
  let total_products = 58
  let capacity_per_box = 15
  let boxes_needed = divide_with_ceil(total_products, capacity_per_box)
  
  // 验证箱子数量
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(60, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(2, remaining_space)
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 1000
  let interest_rate = 8
  let years = 2
  
  // 第一年利息
  let year1_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年利息
  let year2_interest = divide_with_ceil(multiply(year1_total, interest_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  assert_eq(80, year1_interest)
  assert_eq(1080, year1_total)
  assert_eq(87, year2_interest)
  assert_eq(1167, year2_total)
}

test "resource_allocation_planning" {
  // 资源分配规划
  let total_tasks = 95
  let tasks_per_person = 8
  let working_days = 5
  
  // 计算需要的人员数量
  let people_needed = divide_with_ceil(total_tasks, tasks_per_person)
  
  // 计算每个人员的工作天数
  let days_per_person = divide_with_ceil(tasks_per_person, working_days)
  
  // 计算总工作日
  let total_working_days = multiply(people_needed, days_per_person)
  
  assert_eq(12, people_needed)
  assert_eq(2, days_per_person)
  assert_eq(24, total_working_days)
}