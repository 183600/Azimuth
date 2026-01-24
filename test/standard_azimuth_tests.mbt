// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(24, multiply(6, 4))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq_string("Hello, !", greet(""))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-5, add(-2, -3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "real_world_business_calculations" {
  // 实际业务计算测试
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping_cost = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping_cost)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "string_processing_with_unicode" {
  // 字符串处理与Unicode测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, María!", greet("María"))
}

test "complex_calculations" {
  // 复杂计算测试
  let x = 10
  let y = 15
  let z = 20
  
  // 复杂表达式：(x + y) * z / 3
  let result = divide_with_ceil(multiply(add(x, y), z), 3)
  
  assert_eq(167, result)  // ceil((10 + 15) * 20 / 3) = ceil(500/3) = 167
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(1000000, divide_with_ceil(999999, 1))
}

test "practical_application_scenarios" {
  // 实际应用场景测试
  // 库存管理：计算需要的存储箱数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 项目时间线：计算工作日需求
  let tasks_per_day = 5
  let total_tasks = 23
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 能源消耗：计算月度消耗
  let daily_consumption = 120
  let days_in_month = 30
  let monthly_consumption = multiply(daily_consumption, days_in_month)
  
  assert_eq(4, boxes_needed)
  assert_eq(5, workdays_needed)
  assert_eq(3600, monthly_consumption)
}