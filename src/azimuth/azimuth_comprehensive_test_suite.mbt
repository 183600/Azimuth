// Azimuth 综合测试套件 - 标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖核心功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
  assert_eq(0, add(2147483647, -2147483647))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(1, 2147483647))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  let c = 19
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(a, b, c), add(c, b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(a, b, c), multiply(c, b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let unit_price = 89
  let quantity = 12
  let tax_rate = 8
  let shipping = 25
  let discount = 100
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(subtotal, tax, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(1068, subtotal)
  assert_eq(86, tax)
  assert_eq(1179, total_before_discount)
  assert_eq(1079, final_total)
}

test "packaging_and_distribution_calculation" {
  // 包装和分销计算测试
  let total_products = 157
  let box_capacity = 24
  let pallet_capacity = 10
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(boxes_needed, pallet_capacity)
  
  assert_eq(7, boxes_needed)
  assert_eq(1, pallets_needed)
}

test "time_and_resource_management" {
  // 时间和资源管理测试
  let total_tasks = 87
  let tasks_per_day = 8
  let working_days = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(working_days, 8)
  
  // 计算剩余任务时间（如果每天多工作2小时）
  let extra_daily_hours = 2
  let enhanced_daily_capacity = add(tasks_per_day, extra_daily_hours)
  let enhanced_days = divide_with_ceil(total_tasks, enhanced_daily_capacity)
  
  assert_eq(11, working_days)
  assert_eq(88, total_hours)
  assert_eq(9, enhanced_days)
}

test "financial_and_investment_calculations" {
  // 金融和投资计算测试
  let initial_investment = 10000
  let annual_return_rate = 7
  let years = 3
  
  // 计算第一年收益
  let year1_return = divide_with_ceil(multiply(initial_investment, annual_return_rate), 100)
  let year1_total = add(initial_investment, year1_return)
  
  // 计算第二年收益
  let year2_return = divide_with_ceil(multiply(year1_total, annual_return_rate), 100)
  let year2_total = add(year1_total, year2_return)
  
  // 计算第三年收益
  let year3_return = divide_with_ceil(multiply(year2_total, annual_return_rate), 100)
  let year3_total = add(year2_total, year3_return)
  
  assert_eq(700, year1_return)
  assert_eq(10700, year1_total)
  assert_eq(749, year2_return)
  assert_eq(11449, year2_total)
  assert_eq(802, year3_return)
  assert_eq(12251, year3_total)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 测试极端值
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  
  // 测试大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试精度边界
  assert_eq(1, divide_with_ceil(1, 2147483647))
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
}