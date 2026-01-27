// 高质量 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_commutativity" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_properties" {
  // 测试乘法性质
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(100, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(5, -5))
  assert_eq(25, multiply(-5, -5))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
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

test "packaging_optimization" {
  // 包装优化计算
  let items_per_box = 12
  let total_items = 47
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let empty_space = add(total_capacity, -total_items)
  
  assert_eq(1, empty_space)
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "financial_calculation" {
  // 金融计算：复利
  let principal = 1000
  let rate_percent = 5
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "time_management" {
  // 时间管理计算
  let tasks_per_day = 8
  let total_tasks = 35
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(5, days_needed)
  assert_eq(40, total_hours)
}

test "data_analysis" {
  // 数据分析计算
  let data_point1 = 10
  let data_point2 = 20
  let data_point3 = 30
  let data_point4 = 40
  let data_point5 = 50
  
  let sum = add(add(add(add(data_point1, data_point2), data_point3), data_point4), data_point5)
  let average = divide_with_ceil(sum, 5)
  
  assert_eq(150, sum)
  assert_eq(30, average)
}