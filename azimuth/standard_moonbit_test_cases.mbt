// 标准 MoonBit 测试用例
// 覆盖实际应用场景和边界条件

test "financial_calculator_interest_compound" {
  // 金融计算器：复利计算测试
  let principal = 10000  // 本金
  let rate = 5          // 利率 5%
  let years = 3         // 年数
  
  // 计算复利：principal * (1 + rate)^years
  let year1_amount = multiply(principal, add(1, rate))
  let year2_amount = multiply(year1_amount, add(1, rate))
  let year3_amount = multiply(year2_amount, add(1, rate))
  
  assert_eq(1157625, year3_amount)  // 10000 * 1.05^3 = 11576.25，但使用整数运算
}

test "inventory_management_stock_calculation" {
  // 库存管理：库存计算测试
  let initial_stock = 150
  let incoming_shipment = 75
  let sold_items = 98
  let returned_items = 12
  
  // 计算最终库存
  let total_received = add(initial_stock, incoming_shipment)
  let net_sales = add(sold_items, -returned_items)
  let final_stock = add(total_received, -net_sales)
  
  assert_eq(139, final_stock)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 温度转换：摄氏度到华氏度
  let celsius = 25
  // F = C * 9/5 + 32
  let multiplied = multiply(celsius, 9)
  let divided = divide_with_ceil(multiplied, 5)
  let fahrenheit = add(divided, 32)
  
  assert_eq(77, fahrenheit)  // 25°C = 77°F
}

test "batch_processing_page_allocation" {
  // 批处理：页面分配测试
  let total_items = 1000
  let items_per_page = 23
  
  // 计算需要的页数（向上取整）
  let pages_needed = divide_with_ceil(total_items, items_per_page)
  
  assert_eq(44, pages_needed)  // 1000/23 = 43.48，需要44页
}

test "resource_planning_team_distribution" {
  // 资源规划：团队分配测试
  let total_tasks = 87
  let team_size = 8
  
  // 计算每个团队成员的任务数（向上取整确保所有任务被分配）
  let tasks_per_member = divide_with_ceil(total_tasks, team_size)
  
  assert_eq(11, tasks_per_member)  // 87/8 = 10.875，每人11个任务
}

test "user_interface_greeting_localization" {
  // 用户界面：本地化问候测试
  let usernames = ["Alice", "张三", "José", "Mohammed", "Yuki"]
  
  // 测试不同语言用户名的问候
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, Mohammed!", greet("Mohammed"))
  assert_eq_string("Hello, Yuki!", greet("Yuki"))
}

test "mathematical_sequence_fibonacci_calculation" {
  // 数学序列：斐波那契数列计算
  let n1 = 5
  let n2 = 8
  let n3 = 13
  
  // 计算下一个斐波那契数
  let next_fib = add(n2, n3)
  
  // 验证斐波那契性质
  assert_eq(21, next_fib)
  assert_eq(add(n1, n2), n3)
}

test "error_boundary_extreme_inputs" {
  // 错误边界：极端输入测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极端值的加法
  let result1 = add(max_val, -1)
  let result2 = add(min_val, 1)
  
  // 测试极端值的乘法
  let result3 = multiply(max_val, 0)
  let result4 = multiply(min_val, 0)
  
  assert_eq(2147483646, result1)
  assert_eq(-2147483647, result2)
  assert_eq(0, result3)
  assert_eq(0, result4)
}

test "performance_large_number_operations" {
  // 性能：大数运算测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = 3000000
  
  // 大数运算
  let sum_result = add(add(large_num1, large_num2), large_num3)
  let product_result = multiply(1000, 6000)
  let division_result = divide_with_ceil(sum_result, 1000)
  
  assert_eq(6000000, sum_result)
  assert_eq(6000000, product_result)
  assert_eq(6000, division_result)
}

test "real_world_shopping_cart_calculation" {
  // 实际应用：购物车计算测试
  let item1_price = 299
  let item2_price = 199
  let item3_price = 99
  let discount = 50
  let tax_rate = 8  // 8% 税率，表示为 108/100
  
  // 计算小计
  let subtotal = add(add(item1_price, item2_price), item3_price)
  
  // 应用折扣
  let discounted_total = add(subtotal, -discount)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(discounted_total, 8), 100)
  
  // 计算总价
  let final_total = add(discounted_total, tax_amount)
  
  assert_eq(597, discounted_total)  // 299+199+99-50 = 597
  assert_eq(48, tax_amount)        // ceil(597 * 8 / 100) = ceil(47.76) = 48
  assert_eq(645, final_total)      // 597 + 48 = 645
}