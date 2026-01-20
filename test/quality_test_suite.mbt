// 高质量 MoonBit 测试套件
// 包含10个标准测试用例，测试核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(-2, add(3, -5))
  assert_eq(-15, multiply(3, -5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 4)), add(multiply(a, b), multiply(a, 4)))
  
  // 结合律
  assert_eq(add(add(a, b), 4), add(a, add(b, 4)))
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

test "practical_business_calculations" {
  // 测试实际业务计算
  // 订单总价计算
  let unit_price = 199
  let quantity = 3
  let discount = 50
  let shipping = 20
  
  let subtotal = multiply(unit_price, quantity)
  let discounted_total = add(subtotal, -discount)
  let final_total = add(discounted_total, shipping)
  
  assert_eq(627, final_total)
}

test "time_and_date_calculations" {
  // 测试时间日期计算
  // 计算总分钟数
  let hours = 2
  let minutes = 45
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(165, total_minutes)
  
  // 计算时间差
  let start_time = 480  // 8:00 AM
  let end_time = 1020   // 5:00 PM
  let duration = add(end_time, -start_time)
  assert_eq(540, duration)
}

test "statistical_computations" {
  // 测试统计计算
  // 计算平均值
  let values_sum = add(add(add(add(85, 90), 75), 95), 80)
  let count = 5
  let average = values_sum / count
  assert_eq(85, average)
  
  // 计算总和
  assert_eq(425, values_sum)
  
  // 计算最大值和最小值的差
  let max_val = 95
  let min_val = 75
  let range = add(max_val, -min_val)
  assert_eq(20, range)
}

test "geometric_calculations" {
  // 测试几何计算
  // 矩形面积和周长
  let length = 10
  let width = 6
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(60, area)
  assert_eq(32, perimeter)
  
  // 三角形面积（底×高÷2）
  let base = 8
  let height = 5
  let triangle_area = multiply(base, height) / 2
  assert_eq(20, triangle_area)
}

test "currency_conversion_scenarios" {
  // 测试货币转换场景
  let usd_amount = 150
  let exchange_rate = 7  // 1 USD = 7 CNY
  let fee_rate = 2       // 2% 手续费
  
  // 转换为人民币
  let cny_amount = multiply(usd_amount, exchange_rate)
  assert_eq(1050, cny_amount)
  
  // 计算手续费
  let fee = multiply(cny_amount, fee_rate) / 100
  assert_eq(21, fee)
  
  // 计算最终金额
  let final_amount = add(cny_amount, fee)
  assert_eq(1071, final_amount)
}

test "complex_formula_evaluations" {
  // 测试复杂公式计算
  // 计算复利：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  let growth_factor = add(1, multiply(rate, years))
  let total = multiply(principal, growth_factor)
  assert_eq(160000, total)
  
  // 计算 (a + b) × (c + d) - e × f
  let a = 5
  let b = 3
  let c = 4
  let d = 2
  let e = 6
  let f = 3
  
  let result = add(multiply(add(a, b), add(c, d)), multiply(-e, f))
  assert_eq(52, result)
}