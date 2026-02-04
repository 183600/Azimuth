// 标准 Azimuth 核心测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖核心功能

test "add_function_core_functionality" {
  // 测试加法函数的核心功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
  assert_eq(0, add(100, -100))
}

test "multiply_function_core_functionality" {
  // 测试乘法函数的核心功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
}

test "subtract_function_core_functionality" {
  // 测试减法函数的核心功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
}

test "divide_with_ceil_function_core_functionality" {
  // 测试向上取整除法函数的核心功能
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  assert_eq(0, divide_with_ceil(5, 0))   // 除数为0返回0
  assert_eq(-3, divide_with_ceil(-10, 3)) // -3.333... -> -3
}

test "greet_function_core_functionality" {
  // 测试问候函数的核心功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
  
  // 减法的逆运算性质
  assert_eq(a, subtract(add(a, b), b))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价计算
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_optimization_scenario" {
  // 包装优化场景：计算需要的包装箱数量
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_capacity = multiply(boxes_needed, items_per_box)
  let wasted_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)      // ceil(47/12) = 4
  assert_eq(48, total_capacity)   // 4 * 12 = 48
  assert_eq(1, wasted_space)      // 48 - 47 = 1
}

test "financial_compound_interest_scenario" {
  // 金融复利计算场景
  let principal = 1000
  let rate_percent = 5
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let amount_after_year1 = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(amount_after_year1, rate_percent), 100)
  let amount_after_year2 = add(amount_after_year1, year2_interest)
  
  assert_eq(50, year1_interest)
  assert_eq(1050, amount_after_year1)
  assert_eq(53, year2_interest)      // ceil(1050 * 5 / 100) = ceil(52.5) = 53
  assert_eq(1103, amount_after_year2)
}

test "inventory_management_scenario" {
  // 库存管理场景：计算库存变化
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  let new_shipment = 50
  
  // 计算总销售量
  let total_sold = multiply(daily_sales, days_to_simulate)
  
  // 计算发货前库存
  let stock_before_shipment = subtract(initial_stock, total_sold)
  
  // 计算最终库存
  let final_stock = add(stock_before_shipment, new_shipment)
  
  assert_eq(84, total_sold)           // 12 * 7 = 84
  assert_eq(66, stock_before_shipment) // 150 - 84 = 66
  assert_eq(116, final_stock)         // 66 + 50 = 116
}