// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的全面测试，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(-4, add(-10, 6))
  assert_eq(0, add(0, 0))
  
  assert_eq(21, multiply(3, 7))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(0, 100))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.666... -> 4
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_verification" {
  // 验证基本数学性质
  let a = 8
  let b = 13
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_scenario" {
  // 复杂业务场景：电商订单计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_cost = 12
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(add(subtotal, tax_amount), shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(397, total_before_discount)
  assert_eq(377, final_total)
}

test "packaging_and_logistics_calculation" {
  // 包装和物流计算
  let total_items = 127
  let items_per_box = 24
  let boxes_per_pallet = 4
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  
  assert_eq(6, boxes_needed)    // ceil(127/24) = 6
  assert_eq(2, pallets_needed)  // ceil(6/4) = 2
}

test "time_and_resource_management" {
  // 时间和资源管理计算
  let total_tasks = 57
  let tasks_per_day = 8
  let work_hours_per_day = 8
  
  // 计算工作日数
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数
  let total_work_hours = multiply(work_days_needed, work_hours_per_day)
  
  // 计算周数（每周5个工作日）
  let weeks_needed = divide_with_ceil(work_days_needed, 5)
  
  assert_eq(8, work_days_needed)
  assert_eq(64, total_work_hours)
  assert_eq(2, weeks_needed)
}

test "edge_cases_and_boundary_values" {
  // 边界值和特殊情况测试
  // 极小值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 特殊除法情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "financial_calculations" {
  // 金融计算场景
  let principal = 10000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  assert_eq(10500, year1_amount)
  assert_eq(11025, year2_amount)
  assert_eq(11576, year3_amount)
}

test "data_analysis_and_statistics" {
  // 数据分析和统计计算
  let data_points = [15, 22, 18, 25, 20, 17, 23, 19, 21, 16]
  
  // 计算总和（手动累加）
  let sum = add(add(add(add(add(add(add(add(add(data_points[0], data_points[1]), data_points[2]), data_points[3]), data_points[4]), data_points[5]), data_points[6]), data_points[7]), data_points[8]), data_points[9])
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, 10)
  
  // 计算中位数近似值（简化计算）
  let sorted_approx = add(data_points[4], data_points[5]) / 2
  
  assert_eq(196, sum)
  assert_eq(20, average)
  assert_eq(18, sorted_approx)
}