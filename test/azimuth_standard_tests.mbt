// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, add(7, 3))  // 基本加法
  assert_eq(0, add(0, 0))   // 零值加法
  assert_eq(-5, add(-2, -3))  // 负数加法
  assert_eq(5, add(-2, 7))   // 正负数混合
  assert_eq(1000000, add(500000, 500000))  // 大数加法
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(21, multiply(7, 3))  // 基本乘法
  assert_eq(0, multiply(0, 5))   // 零乘法
  assert_eq(-6, multiply(-2, 3))  // 负数乘法
  assert_eq(15, multiply(-3, -5))  // 负负得正
  assert_eq(10000, multiply(100, 100))  // 大数乘法
}

test "divide_with_ceil_function_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))   // 基本向上取整
  assert_eq(3, divide_with_ceil(9, 3))    // 整除情况
  assert_eq(1, divide_with_ceil(1, 100))  // 被除数小于除数
  assert_eq(-3, divide_with_ceil(-10, 3)) // 负数除法
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零的安全处理
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  assert_eq_string("Hello, World!", greet("World"))  // 基本问候
  assert_eq_string("Hello, !", greet(""))            // 空字符串
  assert_eq_string("Hello, 中文!", greet("中文"))    // Unicode字符
  assert_eq_string("Hello, 123!", greet("123"))      // 数字字符串
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))  // 特殊符号
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 8
  let tax_rate = 10
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(360, subtotal)
  assert_eq(36, tax)
  assert_eq(411, total)
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景
  let total_items = 73
  let items_per_box = 12
  let cost_per_box = 5
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算总成本
  let total_cost = multiply(boxes_needed, cost_per_box)
  
  assert_eq(7, boxes_needed)
  assert_eq(35, total_cost)
}

test "time_calculation_scenario" {
  // 时间计算场景
  let tasks_per_day = 6
  let total_tasks = 35
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(workdays_needed, working_days_per_week)
  
  assert_eq(6, workdays_needed)
  assert_eq(2, weeks_needed)
}

test "data_analysis_scenario" {
  // 数据分析场景
  let data_points = 150
  let sample_size = 20
  let threshold = 75
  
  // 计算样本数量
  let sample_count = divide_with_ceil(data_points, sample_size)
  
  // 计算超过阈值的数据点比例
  let above_threshold = 85
  let percentage = divide_with_ceil(multiply(above_threshold, 100), data_points)
  
  assert_eq(8, sample_count)
  assert_eq(57, percentage)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端数值
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, divide_with_ceil(1, 2147483647))
}