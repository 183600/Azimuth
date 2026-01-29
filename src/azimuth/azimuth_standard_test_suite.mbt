// Azimuth 标准测试套件
// 包含10个高质量的 MoonBit 测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 10))
  assert_eq(-8, add(-5, -3))
  assert_eq(-15, multiply(-3, 5))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
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
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "boundary_values_and_edge_cases" {
  // 边界值和边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(max_val, divide_with_ceil(max_val, 1))
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(add(subtotal, tax_amount), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(400, total_before_discount)
  assert_eq(380, final_total)
}

test "inventory_packaging_optimization" {
  // 库存包装优化计算
  let total_items = 127
  let box_capacity = 15
  let layers_per_box = 3
  let items_per_layer = 5
  
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  let layers_needed = divide_with_ceil(total_items, items_per_layer)
  let total_boxes_by_layer = divide_with_ceil(layers_needed, layers_per_box)
  
  assert_eq(9, boxes_needed)
  assert_eq(26, layers_needed)
  assert_eq(9, total_boxes_by_layer)
}

test "string_processing_with_special_characters" {
  // 特殊字符字符串处理
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, C++ Developer!", greet("C++ Developer"))
  assert_eq_string("Hello, a+b=c!", greet("a+b=c"))
  assert_eq_string("Hello, 100%!", greet("100%"))
  assert_eq_string("Hello, #hashtag!", greet("#hashtag"))
}

test "financial_calculation_scenario" {
  // 金融计算场景：复利计算
  let principal = 10000
  let annual_rate = 5
  let years = 3
  
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, annual_rate), 100))
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, annual_rate), 100))
  
  assert_eq(10500, amount_year1)
  assert_eq(11025, amount_year2)
  assert_eq(11576, amount_year3)
}

test "time_and_resource_management" {
  // 时间和资源管理计算
  let total_tasks = 87
  let tasks_per_day = 8
  let working_hours_per_day = 8
  let tasks_per_hour = 2
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_working_hours = multiply(days_needed, working_hours_per_day)
  let hours_needed_by_tasks = divide_with_ceil(total_tasks, tasks_per_hour)
  
  assert_eq(11, days_needed)
  assert_eq(88, total_working_hours)
  assert_eq(44, hours_needed_by_tasks)
}