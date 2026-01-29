// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "subtract_function_basic" {
  // 测试基本的减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "arithmeti_operations_combination" {
  // 测试算术运算的组合使用
  let x = 15
  let y = 7
  let z = 3
  
  // 测试 (x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(63, result)  // (15 + 7) * 3 - ceil(15/7) = 22 * 3 - 3 = 66 - 3 = 63
  
  // 测试 x * y - z * (x + y)
  let result2 = subtract(multiply(x, y), multiply(z, add(x, y)))
  assert_eq(-54, result2)  // 15 * 7 - 3 * (15 + 7) = 105 - 3 * 22 = 105 - 66 = 39
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算简单利息
  let interest = divide_with_ceil(multiply(principal, rate), 100)
  let yearly_total = add(principal, interest)
  let total_after_years = add(principal, multiply(interest, years))
  
  assert_eq(500, interest)
  assert_eq(10500, yearly_total)
  assert_eq(11500, total_after_years)
}

test "inventory_management" {
  // 测试库存管理场景
  let initial_stock = 500
  let daily_sales = 25
  let restock_amount = 100
  let days = 7
  
  // 计算一周后的库存
  let total_sold = multiply(daily_sales, days)
  let stock_before_restock = subtract(initial_stock, total_sold)
  let final_stock = add(stock_before_restock, restock_amount)
  
  assert_eq(175, total_sold)
  assert_eq(325, stock_before_restock)
  assert_eq(425, final_stock)
}

test "temperature_conversion" {
  // 测试温度转换场景（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  
  // 反向转换
  let back_to_celsius = divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9)
  
  assert_eq(77, fahrenheit)
  assert_eq(25, back_to_celsius)
}

test "project_planning" {
  // 测试项目规划场景
  let total_tasks = 120
  let completed_tasks = 45
  let tasks_per_day = 8
  let days_passed = 6
  
  // 计算剩余任务和预计完成时间
  let remaining_tasks = subtract(total_tasks, completed_tasks)
  let tasks_done_so_far = multiply(tasks_per_day, days_passed)
  let days_needed = divide_with_ceil(remaining_tasks, tasks_per_day)
  
  assert_eq(75, remaining_tasks)
  assert_eq(48, tasks_done_so_far)
  assert_eq(10, days_needed)
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  assert_eq_string("Hello,   !", greet("  "))  // 空格
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, 12345!", greet("12345"))  // 数字
  assert_eq_string("Hello, 🚀🌟!", greet("🚀🌟"))  // 表情符号
}

test "complex_business_calculation" {
  // 测试复杂业务计算
  let product_price = 89
  let quantity = 12
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 25
  
  // 计算折扣
  let subtotal = multiply(product_price, quantity)
  let discount = divide_with_ceil(multiply(subtotal, discount_rate), 100)
  let after_discount = subtract(subtotal, discount)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(after_discount, tax_rate), 100)
  
  // 计算总价
  let total = add(add(after_discount, tax), shipping)
  
  assert_eq(1068, subtotal)
  assert_eq(107, discount)
  assert_eq(961, after_discount)
  assert_eq(77, tax)
  assert_eq(1063, total)
}

test "mathematical_series" {
  // 测试数学级数计算
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 6
  
  // 计算等差数列的和：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果
  assert_eq(78, sum)  // 6/2 * (6 + 20) = 3 * 26 = 78
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(0, subtract(large_num, large_num))
}