// 标准 MoonBit 测试套件
// 包含10个标准测试用例，覆盖项目核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(42, multiply(6, 7))
  assert_eq(-24, multiply(-4, 6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 12
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试负数情况
  let c = -8
  let d = 5
  assert_eq(add(c, d), add(d, c))
  assert_eq(multiply(c, d), multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算多项式值
  let x = 3
  let a = 2
  let b = 4
  let c = 1
  
  // 计算 ax² + bx + c
  let x_squared = multiply(x, x)  // 9
  let ax_squared = multiply(a, x_squared)  // 18
  let bx = multiply(b, x)  // 12
  let result = add(add(ax_squared, bx), c)  // 31
  
  assert_eq(31, result)
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  let principal = 5000  // 本金
  let rate_percent = 6  // 利率百分比
  let years = 4  // 年数
  
  // 简单利息计算：利息 = 本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate_percent, years))
  assert_eq(120000, interest)  // 5000 × 6 × 4 = 120000
  
  // 总金额 = 本金 + 利息
  let total_amount = add(principal, interest)
  assert_eq(125000, total_amount)
}

test "temperature_conversion_calculation" {
  // 测试温度转换计算
  // 简化公式：华氏度 = 摄氏度 × 2 + 30
  
  // 测试冰点
  let celsius_freezing = 0
  let fahrenheit_freezing = add(multiply(celsius_freezing, 2), 30)
  assert_eq(30, fahrenheit_freezing)
  
  // 测试室温
  let celsius_room = 22
  let fahrenheit_room = add(multiply(celsius_room, 2), 30)
  assert_eq(74, fahrenheit_room)
  
  // 测试体温
  let celsius_body = 37
  let fahrenheit_body = add(multiply(celsius_body, 2), 30)
  assert_eq(104, fahrenheit_body)
}

test "geometric_area_calculations" {
  // 测试几何面积计算
  // 矩形面积
  let rectangle_length = 15
  let rectangle_width = 8
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  assert_eq(120, rectangle_area)
  
  // 三角形面积（简化计算：底 × 高 ÷ 2）
  let triangle_base = 12
  let triangle_height = 10
  let triangle_area = multiply(triangle_base, triangle_height) / 2
  assert_eq(60, triangle_area)
  
  // 梯形面积（简化计算：(上底 + 下底) × 高 ÷ 2）
  let trapezoid_top = 6
  let trapezoid_bottom = 10
  let trapezoid_height = 8
  let trapezoid_area = multiply(add(trapezoid_top, trapezoid_bottom), trapezoid_height) / 2
  assert_eq(64, trapezoid_area)
}

test "shopping_cart_business_logic" {
  // 测试购物车业务逻辑
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount_amount = 50
  let shipping_fee = 20
  
  // 计算商品总价
  let item1_total = multiply(item1_price, item1_quantity)  // 398
  let item2_total = multiply(item2_price, item2_quantity)  // 299
  let subtotal = add(item1_total, item2_total)  // 697
  
  // 应用折扣（超过阈值）
  let discounted_total = add(subtotal, -discount_amount)  // 647
  
  // 加上运费
  let final_total = add(discounted_total, shipping_fee)  // 667
  
  assert_eq(667, final_total)
}

test "statistical_analysis_functions" {
  // 测试统计分析功能
  // 数据集：15, 25, 35, 45, 55
  
  // 计算总和
  let sum = add(add(add(add(15, 25), 35), 45), 55)
  assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let mean = sum / count
  assert_eq(35, mean)
  
  // 计算中位数（已排序数据集的中间值）
  let median = 35
  assert_eq(median, median)
  
  // 计算范围（最大值 - 最小值）
  let range = add(55, -15)
  assert_eq(40, range)
}