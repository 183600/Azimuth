// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试
// 包含实际的业务场景和边界情况测试

test "add_financial_calculations" {
  // 金融计算场景：月度收入汇总
  let salary = 8000
  let bonus = 1500
  let investment_income = 300
  let total_income = add(add(salary, bonus), investment_income)
  assert_eq(9800, total_income)
}

test "multiply_inventory_calculation" {
  // 库存计算：商品总价
  let unit_price = 45
  let quantity = 12
  let total_cost = multiply(unit_price, quantity)
  assert_eq(540, total_cost)
}

test "greet_multilingual_support" {
  // 多语言支持测试
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, Müller!", greet("Müller"))
  assert_eq_string("Hello, 2024!", greet("2024"))
}

test "divide_with_ceil_resource_planning" {
  // 资源规划：计算需要的团队数量
  let total_tasks = 67
  let tasks_per_team = 8
  let teams_needed = divide_with_ceil(total_tasks, tasks_per_team)
  assert_eq(9, teams_needed)
}

test "complex_order_calculation" {
  // 复杂订单计算：包含多个步骤
  let product_price = 120
  let quantity = 5
  let discount = 50
  let shipping = 25
  
  let subtotal = multiply(product_price, quantity)
  let discounted_total = add(subtotal, -discount)
  let final_total = add(discounted_total, shipping)
  
  assert_eq(600, subtotal)
  assert_eq(550, discounted_total)
  assert_eq(575, final_total)
}

test "divide_with_ceil_time_allocation" {
  // 时间分配：项目完成天数
  let total_hours = 123
  let hours_per_day = 8
  let days_needed = divide_with_ceil(total_hours, hours_per_day)
  assert_eq(16, days_needed)
}

test "mathematical_properties_verification" {
  // 数学性质验证：交换律和结合律
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(a, add(a, 0))
  assert_eq(0, multiply(a, 0))
}

test "greet_edge_cases" {
  // 问候函数边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, a b c!", greet("a b c"))
}

test "divide_with_ceil_negative_handling" {
  // 负数处理：向上取整除法
  assert_eq(-2, divide_with_ceil(-15, 8))
  assert_eq(-1, divide_with_ceil(-5, 6))
  assert_eq(3, divide_with_ceil(-20, -7))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "real_world_shopping_cart" {
  // 真实购物车计算
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  let item3_price = 8
  let item3_qty = 4
  
  let item1_total = multiply(item1_price, item1_qty)
  let item2_total = multiply(item2_price, item2_qty)
  let item3_total = multiply(item3_price, item3_qty)
  let grand_total = add(add(item1_total, item2_total), item3_total)
  
  assert_eq(58, item1_total)
  assert_eq(45, item2_total)
  assert_eq(32, item3_total)
  assert_eq(135, grand_total)
}