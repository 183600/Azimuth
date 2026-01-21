// 高级测试场景 - azimuth 库
// 包含10个针对实际应用场景的测试用例

test "divide_with_ceil_real_world" {
  // 测试 divide_with_ceil 在实际场景中的应用
  // 计算需要多少辆车来运送所有乘客
  let passengers = 47
  let capacity_per_car = 5
  let cars_needed = @azimuth.divide_with_ceil(passengers, capacity_per_car)
  @azimuth.assert_eq(10, cars_needed)
  
  // 计算需要多少页来打印所有文档
  let documents = 23
  let docs_per_page = 4
  let pages_needed = @azimuth.divide_with_ceil(documents, docs_per_page)
  @azimuth.assert_eq(6, pages_needed)
  
  // 测试负数情况
  @azimuth.assert_eq(-9, @azimuth.divide_with_ceil(-47, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(47, 0))
}

test "financial_calculations_advanced" {
  // 高级金融计算测试
  // 计算贷款月供（简化版）
  let principal = 100000
  let annual_rate = 6  // 6% 年利率
  let years = 30
  
  let monthly_rate = annual_rate / 12 / 100
  let total_months = years * 12
  let total_interest = @azimuth.multiply(principal, annual_rate) * years / 100
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = total_payment / total_months
  
  @azimuth.assert_eq(180000, total_interest)
  @azimuth.assert_eq(280000, total_payment)
  @azimuth.assert_eq(777, monthly_payment)  // 向下取整
}

test "inventory_management" {
  // 库存管理测试
  let initial_stock = 150
  let sold_today = 23
  let received_today = 50
  let safety_stock = 20
  
  // 计算当前库存
  let current_stock = @azimuth.add(initial_stock, @azimuth.add(-sold_today, received_today))
  
  // 计算是否需要补货
  let need_reorder = if current_stock < safety_stock { 1 } else { 0 }
  
  // 计算建议订购量
  let suggested_order = if need_reorder == 1 { 
    @azimuth.multiply(safety_stock, 3) - current_stock
  } else { 
    0 
  }
  
  @azimuth.assert_eq(177, current_stock)
  @azimuth.assert_eq(0, need_reorder)
  @azimuth.assert_eq(0, suggested_order)
}

test "temperature_conversion" {
  // 温度转换测试
  let celsius = 25
  
  // 摄氏度转华氏度：F = C × 9/5 + 32（简化为整数运算）
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9（简化为整数运算）
  let back_to_celsius = @azimuth.multiply(@azimuth.add(fahrenheit, -32), 5) / 9
  
  @azimuth.assert_eq(77, fahrenheit)
  @azimuth.assert_eq(25, back_to_celsius)
  
  // 测试冰点
  @azimuth.assert_eq(32, @azimuth.add(@azimuth.multiply(0, 9) / 5, 32))
  
  // 测试沸点
  @azimuth.assert_eq(212, @azimuth.add(@azimuth.multiply(100, 9) / 5, 32))
}

test "geometric_calculations_extended" {
  // 扩展几何计算测试
  let radius = 7
  
  // 计算圆的面积（简化版）：π × r²（使用 π ≈ 22/7）
  let circle_area = @azimuth.multiply(22, @azimuth.multiply(radius, radius)) / 7
  
  // 计算圆的周长（简化版）：2 × π × r
  let circle_circumference = @azimuth.multiply(2, @azimuth.multiply(22, radius)) / 7
  
  // 计算球体体积（简化版）：4/3 × π × r³
  let sphere_volume = @azimuth.multiply(4, @azimuth.multiply(22, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))) / 21
  
  @azimuth.assert_eq(154, circle_area)
  @azimuth.assert_eq(44, circle_circumference)
  @azimuth.assert_eq(1437, sphere_volume)  // 向下取整
}

test "statistical_analysis" {
  // 统计分析测试
  let values = [12, 15, 18, 21, 24, 27, 30]
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(12, 15), 18), 21), 24), 27), 30)
  
  // 计算平均值
  let average = sum / 7
  
  // 计算中位数（数组已排序）
  let median = values[3]  // 索引3（第4个元素）
  
  // 计算范围
  let range = @azimuth.add(30, -12)
  
  @azimuth.assert_eq(147, sum)
  @azimuth.assert_eq(21, average)
  @azimuth.assert_eq(21, median)
  @azimuth.assert_eq(18, range)
}

test "project_management" {
  // 项目管理测试
  let total_tasks = 45
  let completed_tasks = 28
  let team_members = 5
  let days_remaining = 12
  
  // 计算完成百分比
  let completion_percentage = @azimuth.multiply(completed_tasks, 100) / total_tasks
  
  // 计算剩余任务
  let remaining_tasks = @azimuth.add(total_tasks, -completed_tasks)
  
  // 计算每人每天需要完成的任务数
  let tasks_per_person_per_day = @azimuth.divide_with_ceil(remaining_tasks, @azimuth.multiply(team_members, days_remaining))
  
  @azimuth.assert_eq(62, completion_percentage)
  @azimuth.assert_eq(17, remaining_tasks)
  @azimuth.assert_eq(1, tasks_per_person_per_day)
}

test "greeting_internationalization_extended" {
  // 扩展国际化问候测试
  let names = ["Alice", "Bob", "Charlie", "Diana", "Eve"]
  
  // 测试多个名字的问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet(names[0]))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet(names[1]))
  @azimuth.assert_eq_string("Hello, Charlie!", @azimuth.greet(names[2]))
  @azimuth.assert_eq_string("Hello, Diana!", @azimuth.greet(names[3]))
  @azimuth.assert_eq_string("Hello, Eve!", @azimuth.greet(names[4]))
  
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, José!", @azimuth.greet("José"))
  @azimuth.assert_eq_string("Hello, Øyvind!", @azimuth.greet("Øyvind"))
  @azimuth.assert_eq_string("Hello, 北京!", @azimuth.greet("北京"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
}

test "data_structure_operations" {
  // 数据结构操作测试（使用数组索引模拟）
  let data = [10, 20, 30, 40, 50]
  
  // 计算数组元素总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data[0], data[1]), data[2]), data[3]), data[4])
  
  // 计算最大值（简化版）
  let max = if @azimuth.add(data[0], data[1]) > @azimuth.add(data[2], data[3]) { 
    if data[0] > data[1] { data[0] } else { data[1] }
  } else { 
    if data[2] > data[3] { data[2] } else { data[3] }
  }
  let final_max = if max > data[4] { max } else { data[4] }
  
  // 计算平均值
  let average = sum / 5
  
  @azimuth.assert_eq(150, sum)
  @azimuth.assert_eq(50, final_max)
  @azimuth.assert_eq(30, average)
}

test "complex_business_logic" {
  // 复杂业务逻辑测试
  let product_a_price = 120
  let product_a_qty = 3
  let product_b_price = 80
  let product_b_qty = 2
  let discount_threshold = 500
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算小计
  let subtotal_a = @azimuth.multiply(product_a_price, product_a_qty)
  let subtotal_b = @azimuth.multiply(product_b_price, product_b_qty)
  let subtotal = @azimuth.add(subtotal_a, subtotal_b)
  
  // 计算折扣
  let discount_amount = if subtotal > discount_threshold {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算税后总价
  let after_discount = @azimuth.add(subtotal, -discount_amount)
  let tax_amount = @azimuth.multiply(after_discount, tax_rate) / 100
  let total = @azimuth.add(after_discount, tax_amount)
  
  @azimuth.assert_eq(360, subtotal_a)
  @azimuth.assert_eq(160, subtotal_b)
  @azimuth.assert_eq(520, subtotal)
  @azimuth.assert_eq(52, discount_amount)
  @azimuth.assert_eq(468, after_discount)
  @azimuth.assert_eq(37, tax_amount)
  @azimuth.assert_eq(505, total)
}