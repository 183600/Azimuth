// 标准 MoonBit 测试用例 - 为 azimuth 库新增
// 包含核心功能的全面测试，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(7, 3))
  assert_eq(21, multiply(7, 3))
  assert_eq(4, subtract(10, 6))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 15
  let b = 23
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
}

test "zero_element_properties" {
  // 测试零元素性质
  let x = 42
  
  // 加法零元素: x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法零元素: x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
  assert_eq(-21, multiply(-7, 3))
  assert_eq(21, multiply(-7, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 整除情况
  assert_eq(5, divide_with_ceil(15, 3))
  assert_eq(7, divide_with_ceil(21, 3))
  
  // 需要向上取整的情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
}

test "greeting_function_comprehensive" {
  // 测试问候函数的综合功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  let unit_price = 35
  let quantity = 12
  let tax_rate = 8
  let shipping_cost = 18
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_cost = add(add(subtotal, tax_amount), shipping_cost)
  
  assert_eq(420, subtotal)
  assert_eq(34, tax_amount)
  assert_eq(472, total_cost)
}

test "packaging_and_resource_optimization" {
  // 测试包装和资源优化计算
  let total_items = 58
  let items_per_box = 15
  let boxes_per_pallet = 4
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  
  // 计算剩余容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(1, pallets_needed)
  assert_eq(60, total_capacity)
  assert_eq(2, remaining_space)
}

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 7
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 6 * (7 + 8) = 6 * 7 + 6 * 8 = 90
}

test "time_and_project_management" {
  // 测试时间和项目管理计算
  let total_tasks = 67
  let tasks_per_day = 8
  let work_hours_per_day = 8
  let working_days_per_week = 5
  
  // 计算工作日数
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算周数
  let weeks_needed = divide_with_ceil(work_days_needed, working_days_per_week)
  
  // 计算总工作小时数
  let total_work_hours = multiply(work_days_needed, work_hours_per_day)
  
  assert_eq(9, work_days_needed)
  assert_eq(2, weeks_needed)
  assert_eq(72, total_work_hours)
}