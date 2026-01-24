// 创意综合测试用例
// 针对 azimuth 库的高级功能和实际应用场景测试

test "temperature_conversion_calculator" {
  // 温度转换计算器：摄氏度到华氏度
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  // 验证转换结果
  assert_eq(77, fahrenheit)  // 25°C = 77°F
  
  // 华氏度到摄氏度
  let fahrenheit_back = 77
  let celsius_back = multiply(fahrenheit_back - 32, 5) / 9
  assert_eq(25, celsius_back)
}

test "loan_interest_calculator" {
  // 贷款利息计算器
  let principal = 10000  // 本金
  let annual_rate = 5    // 年利率5%
  let years = 3          // 3年
  
  // 简单利息计算
  let simple_interest = multiply(principal, annual_rate * years) / 100
  let total_payment = add(principal, simple_interest)
  
  assert_eq(1500, simple_interest)
  assert_eq(11500, total_payment)
}

test "pizza_order_calculator" {
  // 披萨订单计算器
  let pizza_price = 12
  let topping_price = 2
  let delivery_fee = 5
  let pizzas = 3
  let extra_toppings = 4
  
  // 计算总价
  let pizza_cost = multiply(pizza_price, pizzas)
  let topping_cost = multiply(topping_price, extra_toppings)
  let subtotal = add(pizza_cost, topping_cost)
  let total = add(subtotal, delivery_fee)
  
  assert_eq(36, pizza_cost)
  assert_eq(8, topping_cost)
  assert_eq(44, subtotal)
  assert_eq(49, total)
}

test "classroom_seating_arrangement" {
  // 教室座位安排计算
  let students = 47
  let seats_per_row = 6
  let rows_needed = divide_with_ceil(students, seats_per_row)
  
  // 计算空余座位
  let total_seats = multiply(rows_needed, seats_per_row)
  let empty_seats = add(total_seats, -students)
  
  assert_eq(8, rows_needed)  // ceil(47/6) = 8
  assert_eq(48, total_seats) // 8 * 6 = 48
  assert_eq(1, empty_seats)  // 48 - 47 = 1
}

test "digital_storage_conversion" {
  // 数字存储转换计算
  let megabytes = 850
  let kilobytes_per_mb = 1024
  
  // MB 转换为 KB
  let total_kilobytes = multiply(megabytes, kilobytes_per_mb)
  
  // 计算需要多少张1.44MB软盘
  let floppy_disk_kb = 1474  // 1.44MB = 1474.56KB，向下取整
  let floppy_count = divide_with_ceil(total_kilobytes, floppy_disk_kb)
  
  assert_eq(870400, total_kilobytes)
  assert_eq(591, floppy_count)  // ceil(870400/1474) = 591
}

test "fitness_calorie_tracker" {
  // 健身卡路里追踪器
  let calories_per_pizza_slice = 285
  let calories_per_burger = 540
  let calories_per_salad = 150
  let daily_target = 2000
  
  let pizza_slices = 2
  let burgers = 1
  let salads = 1
  
  // 计算总卡路里
  let pizza_calories = multiply(calories_per_pizza_slice, pizza_slices)
  let burger_calories = multiply(calories_per_burger, burgers)
  let salad_calories = multiply(calories_per_salad, salads)
  let total_calories = add(add(pizza_calories, burger_calories), salad_calories)
  
  // 计算剩余卡路里
  let remaining_calories = add(daily_target, -total_calories)
  
  assert_eq(570, pizza_calories)
  assert_eq(540, burger_calories)
  assert_eq(150, salad_calories)
  assert_eq(1260, total_calories)
  assert_eq(740, remaining_calories)
}

test "travel_budget_planner" {
  // 旅行预算规划器
  let flight_cost = 450
  let hotel_per_night = 120
  let food_per_day = 80
  let activities_budget = 200
  let nights = 4
  let days = 5
  
  // 计算各项费用
  let hotel_total = multiply(hotel_per_night, nights)
  let food_total = multiply(food_per_day, days)
  let accommodation_and_food = add(hotel_total, food_total)
  let subtotal = add(accommodation_and_food, activities_budget)
  let total_budget = add(subtotal, flight_cost)
  
  assert_eq(480, hotel_total)
  assert_eq(400, food_total)
  assert_eq(880, accommodation_and_food)
  assert_eq(1080, subtotal)
  assert_eq(1530, total_budget)
}

test "gaming_score_system" {
  // 游戏得分系统
  let base_score = 100
  let level_multiplier = 15
  let bonus_per_life = 50
  let current_level = 8
  let remaining_lives = 3
  
  // 计算最终得分
  let level_bonus = multiply(current_level, level_multiplier)
  let life_bonus = multiply(remaining_lives, bonus_per_life)
  let final_score = add(add(base_score, level_bonus), life_bonus)
  
  assert_eq(120, level_bonus)
  assert_eq(150, life_bonus)
  assert_eq(370, final_score)
}

test "international_greeting_system" {
  // 国际问候系统
  let english_name = "Alice"
  let chinese_name = "小明"
  let spanish_name = "Carlos"
  let emoji_name = "🌟"
  
  // 测试多语言问候
  let english_greeting = greet(english_name)
  let chinese_greeting = greet(chinese_name)
  let spanish_greeting = greet(spanish_name)
  let emoji_greeting = greet(emoji_name)
  
  assert_eq_string("Hello, Alice!", english_greeting)
  assert_eq_string("Hello, 小明!", chinese_greeting)
  assert_eq_string("Hello, Carlos!", spanish_greeting)
  assert_eq_string("Hello, 🌟!", emoji_greeting)
}

test "resource_allocation_optimizer" {
  // 资源分配优化器
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  let emergency_fund_percentage = 10
  
  // 计算项目总成本
  let project_costs = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算应急基金
  let emergency_fund = divide_with_ceil(multiply(total_budget, emergency_fund_percentage), 100)
  
  // 计算可用预算
  let available_budget = add(total_budget, -emergency_fund)
  
  // 计算剩余资金
  let remaining_funds = add(available_budget, -project_costs)
  
  assert_eq(3500, project_costs)
  assert_eq(500, emergency_fund)
  assert_eq(4500, available_budget)
  assert_eq(1000, remaining_funds)
}