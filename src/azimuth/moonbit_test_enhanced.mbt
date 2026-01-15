// 增强的 MoonBit 测试用例
// 测试 azimuth 包中的核心功能的各种场景

test "calculator_basic_operations" {
  // 基本计算器操作测试
  let a = 15
  let b = 7
  
  // 加法测试
  assert_eq(22, add(a, b))
  
  // 乘法测试
  assert_eq(105, multiply(a, b))
  
  // 复合运算：(a + b) * 2
  let sum = add(a, b)
  let doubled = multiply(sum, 2)
  assert_eq(44, doubled)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度测试
  // 使用简化公式：F = C × 9/5 + 32
  // 由于只有整数运算，我们使用：F = C × 2 + 30
  
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 20
  let room_fahrenheit = add(multiply(room_celsius, 2), 30)
  assert_eq(70, room_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
}

test "shopping_cart_total_calculation" {
  // 购物车总价计算测试
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算每种商品的总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = add(add(total1, total2), shipping_fee)
  
  // 验证总价：199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
  assert_eq(717, cart_total)
}

test "simple_interest_calculation" {
  // 简单利息计算测试
  // 公式：利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate_percent = 5  // 5%
  let years = 3
  
  // 计算利息（利率转换为小数：5% = 5/100）
  let interest = multiply(principal, multiply(rate_percent, years)) / 100
  
  // 计算总金额
  let total_amount = add(principal, interest)
  
  // 验证：10000 + 10000*5*3/100 = 10000 + 1500 = 11500
  assert_eq(11500, total_amount)
}

test "geometric_area_calculations" {
  // 几何面积计算测试
  
  // 正方形面积：边长 × 边长
  let square_side = 5
  let square_area = multiply(square_side, square_side)
  assert_eq(25, square_area)
  
  // 矩形面积：长 × 宽
  let rectangle_length = 8
  let rectangle_width = 6
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  assert_eq(48, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = multiply(triangle_base, triangle_height) / 2
  assert_eq(30, triangle_area)
}

test "greeting_function_edge_cases" {
  // 问候函数边界情况测试
  
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 包含空格的名称
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  
  // 包含特殊字符的名称
  assert_eq_string("Hello, user@123!", greet("user@123"))
  
  // 包含Unicode字符的名称
  assert_eq_string("Hello, 张三!", greet("张三"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  let z = 5
  
  // 加法交换律：x + y = y + x
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律：x * y = y * x
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律：(x + y) + z = x + (y + z)
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律：(x * y) * z = x * (y * z)
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律：x * (y + z) = x * y + x * z
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "boundary_value_operations" {
  // 边界值操作测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法
  assert_eq(max_int, add(max_int, 1))  // 溢出保护
  assert_eq(min_int, add(min_int, -1))  // 溢出保护
  assert_eq(-1, add(max_int, min_int))
  
  // 边界值乘法
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
  assert_eq(max_int, multiply(max_int, 1))
  assert_eq(min_int, multiply(min_int, 1))
  assert_eq(min_int, multiply(min_int, -1))  // 特殊情况：-2147483648 * -1 溢出
}

test "time_conversion_calculations" {
  // 时间转换计算测试
  
  // 小时和分钟转换为总分钟数
  let hours = 2
  let minutes = 45
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(165, total_minutes)
  
  // 天数转换为总小时数
  let days = 3
  let total_hours = multiply(days, 24)
  assert_eq(72, total_hours)
  
  // 工作周计算（5天工作制）
  let work_weeks = 4
  let work_days = multiply(work_weeks, 5)
  let work_hours = multiply(work_days, 8)
  assert_eq(160, work_hours)
}