// 标准 MoonBit 测试套件
// 针对 azimuth 库核心功能的高质量测试用例

// 导入 azimuth 库
use azimuth

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, azimuth.add(3, 7))
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(-2, azimuth.subtract(3, 5))
  assert_eq(4, azimuth.divide_with_ceil(10, 3))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 零元素性质
  assert_eq(0, azimuth.multiply(a, 0))
  assert_eq(a, azimuth.add(a, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, azimuth.divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, azimuth.divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, azimuth.divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(4, azimuth.divide_with_ceil(-11, -3))   // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, azimuth.divide_with_ceil(5, 0))   // 除数为零返回0
  assert_eq(0, azimuth.divide_with_ceil(0, 5))   // 零除以任何数返回0
}

test "greet_function_multilingual_support" {
  // 测试多语言问候功能
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, María!", azimuth.greet("María"))
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = azimuth.multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = azimuth.divide_with_ceil(azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)  // 25 * 8 = 200
  assert_eq(16, tax)        // ceil(200 * 8 / 100) = ceil(16) = 16
  assert_eq(231, total)     // 200 + 16 + 15 = 231
}

test "inventory_packaging_scenario" {
  // 库存包装场景
  let total_items = 47
  let items_per_box = 12
  
  // 计算需要的包装箱数量
  let boxes_needed = azimuth.divide_with_ceil(total_items, items_per_box)
  
  // 计算剩余空间
  let total_capacity = azimuth.multiply(boxes_needed, items_per_box)
  let empty_space = azimuth.subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)    // ceil(47/12) = 4
  assert_eq(48, total_capacity) // 4 * 12 = 48
  assert_eq(1, empty_space)     // 48 - 47 = 1
}

test "project_timeline_calculation" {
  // 项目时间线计算
  let total_tasks = 35
  let tasks_per_day = 8
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = azimuth.divide_with_ceil(workdays_needed, working_days_per_week)
  
  assert_eq(5, workdays_needed)  // ceil(35/8) = 5
  assert_eq(1, weeks_needed)     // ceil(5/5) = 1
}

test "financial_budget_analysis" {
  // 财务预算分析
  let monthly_income = 5000
  let rent = 1500
  let utilities = 200
  let food = 600
  let transportation = 300
  let savings_goal = 1000
  
  // 计算总支出
  let total_expenses = azimuth.add(azimuth.add(azimuth.add(rent, utilities), food), transportation)
  
  // 计算可用于储蓄的金额
  let available_for_savings = azimuth.subtract(monthly_income, total_expenses)
  
  // 检查是否达到储蓄目标
  let savings_shortfall = azimuth.subtract(savings_goal, available_for_savings)
  
  assert_eq(2600, total_expenses)     // 1500 + 200 + 600 + 300 = 2600
  assert_eq(2400, available_for_savings) // 5000 - 2600 = 2400
  assert_eq(-1400, savings_shortfall)   // 1000 - 2400 = -1400 (超额完成)
}

test "data_processing_simulation" {
  // 数据处理模拟
  let base_value = 42
  let processing_cycles = 100
  let multiplier = 3
  let offset = 10
  
  // 模拟数据处理循环
  let processed_value = azimuth.multiply(base_value, processing_cycles)
  let enhanced_value = azimuth.multiply(processed_value, multiplier)
  let final_result = azimuth.add(enhanced_value, offset)
  
  assert_eq(4200, processed_value)    // 42 * 100 = 4200
  assert_eq(12600, enhanced_value)   // 4200 * 3 = 12600
  assert_eq(12610, final_result)     // 12600 + 10 = 12610
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的各种情况
  assert_eq(0, azimuth.divide_with_ceil(100, 0))
  assert_eq(0, azimuth.divide_with_ceil(-100, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num = 1000000
  assert_eq(2000000, azimuth.add(large_num, large_num))
  assert_eq(1000000000000, azimuth.multiply(large_num, large_num))
  
  // 测试极值运算
  assert_eq(-1000000, azimuth.subtract(0, large_num))
  assert_eq(1, azimuth.divide_with_ceil(large_num, large_num))
}