// 标准 MoonBit 测试套件
// 针对 azimuth 库核心功能的综合测试

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
  
  // 零除数安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, 🚀🛸!", greet("🚀🛸"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let interest_rate = 5
  let years = 3
  
  // 计算复利（简化版本，使用向上取整）
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 25
  let days = 15
  let new_shipment = 300
  
  // 计算销售后的库存
  let total_sales = multiply(daily_sales, days)
  let stock_after_sales = add(initial_stock, -total_sales)
  
  // 收到新货后的库存
  let final_stock = add(stock_after_sales, new_shipment)
  
  assert_eq(375, total_sales)
  assert_eq(125, stock_after_sales)
  assert_eq(425, final_stock)
}

test "mathematical_distributive_law" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  assert_eq(140, left_side)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_budget = 50000
  let team_members = 8
  let project_duration_days = 30
  
  // 计算每人每日预算
  let daily_budget_per_person = divide_with_ceil(
    divide_with_ceil(total_budget, team_members), 
    project_duration_days
  )
  
  // 计算总项目成本（可能因向上取整而增加）
  let actual_total_cost = multiply(
    multiply(daily_budget_per_person, team_members), 
    project_duration_days
  )
  
  assert_eq(209, daily_budget_per_person)
  assert_eq(50160, actual_total_cost)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, This is a very long name!", greet("This is a very long name"))
  assert_eq_string("Hello, line\nbreak!", greet("line\nbreak"))
  assert_eq_string("Hello, tab\tcharacter!", greet("tab\tcharacter"))
}

test "comprehensive_business_workflow" {
  // 综合业务工作流测试
  let product_cost = 150
  let quantity = 12
  let tax_rate = 8
  let shipping_per_item = 10
  let discount_threshold = 2000
  let discount_amount = 100
  
  // 计算小计
  let subtotal = multiply(product_cost, quantity)
  
  // 计算运费
  let total_shipping = multiply(shipping_per_item, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, total_shipping), tax_amount)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(1800, subtotal)
  assert_eq(120, total_shipping)
  assert_eq(144, tax_amount)
  assert_eq(2064, pre_discount_total)
  assert_eq(1964, final_total)
}