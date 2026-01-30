// 标准 MoonBit 测试用例 - Azimuth 库增强测试
// 包含10个高质量的测试用例，覆盖核心功能和实际应用场景

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, add(7, 3))           // 基本加法
  assert_eq(0, add(0, 0))            // 零值加法
  assert_eq(-5, add(-2, -3))         // 负数加法
  assert_eq(5, add(-2, 7))           // 正负数混合
  assert_eq(1000, add(500, 500))     // 大数加法
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(21, multiply(7, 3))      // 基本乘法
  assert_eq(0, multiply(0, 5))       // 零乘法
  assert_eq(-6, multiply(-2, 3))     // 负数乘法
  assert_eq(12, multiply(-3, -4))    // 负负得正
  assert_eq(10000, multiply(100, 100)) // 大数乘法
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))                    // 空字符串
  assert_eq_string("Hello, 中文!", greet("中文"))             // Unicode字符
  assert_eq_string("Hello, 🚀!", greet("🚀"))                // Emoji
  assert_eq_string("Hello, John Doe!", greet("John Doe"))     // 长字符串
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))    // 基本向上取整
  assert_eq(3, divide_with_ceil(9, 3))     // 整除情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为零
  assert_eq(-3, divide_with_ceil(-10, 3))  // 负数除法
  assert_eq(4, divide_with_ceil(-10, -3))  // 负负除法
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(a, add(a, 0))
  
  // 乘法单位元
  assert_eq(a, multiply(a, 1))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_cost = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax_amount)
  assert_eq(307, total_cost)
}

test "inventory_management_scenario" {
  // 库存管理场景
  let initial_stock = 120
  let daily_sales = 8
  let days = 7
  
  // 计算总销售量
  let total_sold = multiply(daily_sales, days)
  // 计算剩余库存
  let remaining_stock = add(initial_stock, -total_sold)
  
  assert_eq(56, total_sold)
  assert_eq(64, remaining_stock)
}

test "packaging_optimization" {
  // 包装优化场景
  let total_items = 47
  let box_capacity = 12
  let pallet_capacity = 10
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(boxes_needed, pallet_capacity)
  
  assert_eq(4, boxes_needed)
  assert_eq(1, pallets_needed)
}

test "time_calculation_scenario" {
  // 时间计算场景
  let total_tasks = 35
  let tasks_per_hour = 6
  let work_hours_per_day = 8
  
  // 计算需要的工作小时数
  let work_hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  // 计算需要的工作天数
  let work_days_needed = divide_with_ceil(work_hours_needed, work_hours_per_day)
  
  assert_eq(6, work_hours_needed)
  assert_eq(1, work_days_needed)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, add(0, 1))
  assert_eq(0, multiply(1, 0))
  
  // 特殊字符串处理
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, 	!", greet("\t"))
}