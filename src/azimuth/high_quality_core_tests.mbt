// 标准 MoonBit 核心测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "addition_commutative_law" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
}

test "multiplication_identity_property" {
  // 测试乘法单位元性质：a * 1 = a
  assert_eq(0, multiply(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(5, multiply(5, 1))
  assert_eq(10, multiply(10, 1))
  assert_eq(-15, multiply(-15, 1))
  assert_eq(0, multiply(5, 0))
  assert_eq(-25, multiply(-5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))   // 10/3 = 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.333... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0返回0
  assert_eq(0, divide_with_ceil(0, 5))     // 0除以任何数返回0
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 4 * (7 + 9) = 4 * 7 + 4 * 9 = 64
}

test "practical_business_calculation" {
  // 实际业务计算：订单总价计算
  let unit_price = 35
  let quantity = 6
  let tax_rate = 8
  let shipping_cost = 12
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_cost = add(add(subtotal, tax_amount), shipping_cost)
  
  assert_eq(210, subtotal)
  assert_eq(17, tax_amount)
  assert_eq(239, total_cost)
}

test "packaging_optimization_calculation" {
  // 包装优化计算：计算需要的包装箱数量
  let total_items = 47
  let items_per_small_box = 8
  let items_per_large_box = 15
  
  // 计算使用小箱子需要的数量
  let small_boxes_needed = divide_with_ceil(total_items, items_per_small_box)
  
  // 计算使用大箱子需要的数量
  let large_boxes_needed = divide_with_ceil(total_items, items_per_large_box)
  
  assert_eq(6, small_boxes_needed)   // ceil(47/8) = 6
  assert_eq(4, large_boxes_needed)   // ceil(47/15) = 4
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  let x = 12
  let y = 8
  let z = 5
  
  // 测试表达式：(x + y) * z - x / y
  let result1 = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  
  // 测试嵌套表达式：x * (y + z) + y * (x - z)
  let result2 = add(multiply(x, add(y, z)), multiply(y, add(x, -z)))
  
  assert_eq(99, result1)  // (12 + 8) * 5 - ceil(12/8) = 20*5 - 2 = 100 - 2 = 98
  assert_eq(156, result2) // 12 * (8 + 5) + 8 * (12 - 5) = 12*13 + 8*7 = 156 + 56 = 212
}

test "time_and_resource_planning" {
  // 时间和资源规划计算
  let total_tasks = 65
  let tasks_per_day = 7
  let work_hours_per_day = 8
  let days_per_week = 5
  
  // 计算需要的工作天数
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数
  let total_work_hours = multiply(work_days_needed, work_hours_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, days_per_week)
  
  assert_eq(10, work_days_needed)  // ceil(65/7) = 10
  assert_eq(80, total_work_hours)  // 10 * 8 = 80
  assert_eq(2, weeks_needed)       // ceil(10/5) = 2
}

test "edge_cases_and_error_handling" {
  // 边界情况和错误处理测试
  // 测试极值运算
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
  assert_eq(0, multiply(0, 2147483647))        // 零乘以最大值
  assert_eq(0, divide_with_ceil(0, 2147483647)) // 零除以最大值
  
  // 测试除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试负数运算
  assert_eq(0, add(5, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-2, divide_with_ceil(-8, 4))
}