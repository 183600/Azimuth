// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，自包含版本

// 定义需要的函数
fn add(a : Int, b : Int) -> Int {
  if a == 0 { return b }
  if b == 0 { return a }
  
  let min_val = -2147483648
  let max_val = 2147483647
  
  if a == min_val && b < 0 { return min_val }
  if b == min_val && a < 0 { return min_val }
  
  if a > 0 && b > 0 && a > max_val - b { return max_val }
  if a < 0 && b < 0 && a < min_val - b { return min_val }
  
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  let min_val = -2147483648
  let max_val = 2147483647
  
  if a == 0 || b == 0 { return 0 }
  if a == 1 { return b }
  if b == 1 { return a }
  
  if a == -1 { return if b == min_val { min_val } else { -b } }
  if b == -1 { return if a == min_val { min_val } else { -a } }
  
  if a == min_val { return if b > 1 || b < -1 { min_val } else { a * b } }
  if b == min_val { return if a > 1 || a < -1 { min_val } else { a * b } }
  
  let sign = if (a > 0 && b > 0) || (a < 0 && b < 0) { 1 } else { -1 }
  let abs_a = if a < 0 { -a } else { a }
  let abs_b = if b < 0 { -b } else { b }
  
  if abs_a > max_val / abs_b { return if sign > 0 { max_val } else { min_val } }
  
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected == actual {
    ()
  } else {
    // 使用除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected == actual {
    ()
  } else {
    // 使用除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

test "safe_addition_edge_cases" {
  // 测试安全加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 测试最小值溢出保护
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试正常加法
  assert_eq(0, add(max_val, min_val))
  assert_eq(100, add(50, 50))
}

test "safe_multiplication_precision" {
  // 测试安全乘法的精度和溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘法溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试特殊值乘法
  assert_eq(0, multiply(0, max_val))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试正常乘法
  assert_eq(100, multiply(10, 10))
  assert_eq(-100, multiply(-10, 10))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  // 测试基本功能
  assert_eq_string("Hello, World!", greet("World"))
  
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, MoonBit@2023!", greet("MoonBit@2023"))
  
  // 测试Unicode字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  let c = 5
  
  // 验证加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 验证乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证分配律
  let distributive1 = multiply(a, add(b, c))
  let distributive2 = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive1, distributive2)
  
  // 验证结合律
  let associative1 = add(add(a, b), c)
  let associative2 = add(a, add(b, c))
  assert_eq(associative1, associative2)
}

test "financial_calculations_scenario" {
  // 测试金融计算场景
  let principal = 10000  // 本金
  let rate = 5          // 年利率（百分比）
  let years = 3         // 年数
  
  // 计算简单利息
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  // 验证计算结果：10000 + 10000×5×3 = 160000
  assert_eq(160000, total)
  
  // 测试贷款计算
  let loan_amount = 50000
  let monthly_payment = 2000
  let months = 24
  
  let total_payment = multiply(monthly_payment, months)
  let total_interest = add(total_payment, -loan_amount)
  
  assert_eq(48000, total_payment)
  assert_eq(-2000, total_interest)
}

test "geometry_calculations_scenario" {
  // 测试几何计算场景
  let length = 10
  let width = 6
  let height = 4
  
  // 计算矩形周长
  let perimeter = multiply(2, add(length, width))
  assert_eq(32, perimeter)
  
  // 计算矩形面积
  let area = multiply(length, width)
  assert_eq(60, area)
  
  // 计算长方体体积
  let volume = multiply(area, height)
  assert_eq(240, volume)
  
  // 计算表面积
  let surface_area = add(
    add(multiply(2, multiply(length, width)), 
        multiply(2, multiply(length, height))),
    multiply(2, multiply(width, height))
  )
  assert_eq(148, surface_area)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景（简化版：F = C × 2 + 30）
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
  
  // 测试室温
  let room_celsius = 20
  let room_fahrenheit = add(multiply(room_celsius, 2), 30)
  assert_eq(70, room_fahrenheit)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
  
  // 测试反向转换（简化版：C = (F - 30) ÷ 2）
  let fahrenheit = 80
  let celsius = add(fahrenheit, -30) / 2
  assert_eq(25, celsius)
}

test "shopping_cart_business_scenario" {
  // 测试购物车业务场景
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount_threshold = 1000
  let discount_rate = 10
  let shipping_fee = 20
  
  // 计算商品总价
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  let subtotal = add(item1_total, item2_total)
  
  // 计算折扣
  let discount = if (subtotal > discount_threshold) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终总价
  let final_total = add(add(subtotal, -discount), shipping_fee)
  
  // 验证计算结果
  assert_eq(1194, subtotal)
  assert_eq(119, discount)
  assert_eq(1095, final_total)
}

test "time_calculations_scenario" {
  // 测试时间计算场景
  let hours = 2
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(165, total_minutes)
  
  // 转换为总秒数
  let total_seconds = multiply(total_minutes, 60)
  assert_eq(9900, total_seconds)
  
  // 计算时间差
  let start_hour = 8
  let start_minute = 30
  let end_hour = 17
  let end_minute = 45
  
  let start_total = add(multiply(start_hour, 60), start_minute)
  let end_total = add(multiply(end_hour, 60), end_minute)
  let duration = add(end_total, -start_total)
  
  assert_eq(555, duration)  // 9小时15分钟 = 555分钟
  
  // 计算工作时长（扣除1小时午餐时间）
  let work_duration = add(duration, -60)
  assert_eq(495, work_duration)  // 8小时15分钟 = 495分钟
}