// Azimuth 综合标准测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(15, add(7, 8))
  assert_eq(15, add(8, 7))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_properties" {
  // 测试乘法性质
  assert_eq(0, multiply(5, 0))  // 零元素性质
  assert_eq(0, multiply(0, 5))
  assert_eq(7, multiply(7, 1))  // 单位元性质
  assert_eq(7, multiply(1, 7))
  assert_eq(-21, multiply(3, -7))  // 负数乘法
  assert_eq(21, multiply(-3, -7))  // 负负得正
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))   // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -3.666... -> -4
  assert_eq(4, divide_with_ceil(-10, -3))  // 3.333... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为零
  assert_eq(0, divide_with_ceil(0, 5))     // 被除数为零
  assert_eq(0, divide_with_ceil(0, 0))     // 两者都为零
}

test "greet_function_various_inputs" {
  // 测试各种输入的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello,  !", greet(" "))  // 空格
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, 世界!", greet("世界"))  // 中文
  assert_eq_string("Hello, 🚀!", greet("🚀"))  // 表情符号
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算总价（含运费）
  let total_before_discount = add(subtotal, shipping)
  // 应用折扣
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_and_shipping_calculation" {
  // 包装和运输计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算运输成本（每箱5元）
  let shipping_cost_per_box = 5
  let total_shipping_cost = multiply(boxes_needed, shipping_cost_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  assert_eq(20, total_shipping_cost)  // 4 * 5 = 20
}

test "mathematical_distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "time_and_resource_calculation" {
  // 时间和资源计算
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时（每天8小时）
  let total_hours = multiply(days_needed, 8)
  
  // 计算总工作分钟
  let total_minutes = multiply(total_hours, 60)
  
  assert_eq(5, days_needed)  // ceil(35/8) = 5
  assert_eq(40, total_hours)  // 5 * 8 = 40
  assert_eq(2400, total_minutes)  // 40 * 60 = 2400
}

test "financial_calculation_with_interest" {
  // 金融计算：带利息的贷款
  let principal = 1000
  let interest_rate = 5  // 5%
  let loan_term_years = 3
  
  // 简单利息计算（每年）
  let annual_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let total_interest = multiply(annual_interest, loan_term_years)
  let total_repayment = add(principal, total_interest)
  
  assert_eq(50, annual_interest)  // ceil(1000 * 5 / 100) = 50
  assert_eq(150, total_interest)  // 50 * 3 = 150
  assert_eq(1150, total_repayment)  // 1000 + 150 = 1150
}

test "data_processing_and_analysis" {
  // 数据处理和分析
  let data_points = 1000
  let sample_size = 75
  let processing_time_per_sample = 3  // 分钟
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(data_points, sample_size)
  
  // 计算总处理时间
  let total_processing_time = multiply(batches_needed, processing_time_per_sample)
  
  // 计算每小时可以处理的样本数
  let samples_per_hour = divide_with_ceil(60, processing_time_per_sample)
  
  assert_eq(14, batches_needed)  // ceil(1000/75) = 14
  assert_eq(42, total_processing_time)  // 14 * 3 = 42
  assert_eq(20, samples_per_hour)  // ceil(60/3) = 20
}