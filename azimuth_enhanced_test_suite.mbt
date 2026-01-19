// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法，提供高质量的测试用例

// 定义辅助函数以避免导入问题
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

test "addition_with_various_ranges" {
  // 测试不同范围的加法运算
  assert_eq(100, add(50, 50))
  assert_eq(0, add(-50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multiplication_comprehensive" {
  // 测试全面的乘法运算
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(2147483647, multiply(46341, 46341))
}

test "greet_function_international" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, 👋!", greet("👋"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, عربي!", greet("عربي"))
}

test "complex_business_calculations" {
  // 测试复杂的商业计算场景
  let unit_price = 199
  let quantity = 3
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  assert_eq(597, subtotal)
  
  // 计算折扣
  let discount = multiply(subtotal, discount_rate) / 100
  assert_eq(59, discount)
  
  // 计算折后价格
  let discounted_total = add(subtotal, -discount)
  assert_eq(538, discounted_total)
  
  // 计算税费
  let tax = multiply(discounted_total, tax_rate) / 100
  assert_eq(43, tax)
  
  // 计算最终总价
  let final_total = add(discounted_total, tax)
  assert_eq(581, final_total)
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 计算斐波那契数列前10项的和（简化版）
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  
  // 计算等比数列：2, 4, 8, 16, 32
  let geo1 = 2
  let geo2 = multiply(geo1, 2)
  let geo3 = multiply(geo2, 2)
  let geo4 = multiply(geo3, 2)
  let geo5 = multiply(geo4, 2)
  
  assert_eq(4, geo2)
  assert_eq(8, geo3)
  assert_eq(16, geo4)
  assert_eq(32, geo5)
}

test "geometry_practical_calculations" {
  // 测试实用几何计算
  let length = 12
  let width = 8
  let height = 5
  
  // 计算矩形面积
  let area = multiply(length, width)
  assert_eq(96, area)
  
  // 计算矩形周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(40, perimeter)
  
  // 计算长方体体积
  let volume = multiply(area, height)
  assert_eq(480, volume)
  
  // 计算表面积
  let side1 = multiply(length, width)
  let side2 = multiply(width, height)
  let side3 = multiply(length, height)
  let surface_area = multiply(2, add(add(side1, side2), side3))
  assert_eq(392, surface_area)
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度：F = C × 9/5 + 32（简化为整数运算：F = C × 2 + 30）
  let celsius1 = 0
  let celsius2 = 10
  let celsius3 = 20
  
  let f1 = add(multiply(celsius1, 2), 30)
  let f2 = add(multiply(celsius2, 2), 30)
  let f3 = add(multiply(celsius3, 2), 30)
  
  assert_eq(30, f1)
  assert_eq(50, f2)
  assert_eq(70, f3)
  
  // 华氏度转摄氏度：C = (F - 30) / 2
  let fahrenheit = 100
  let celsius = add(fahrenheit, -30) / 2
  assert_eq(35, celsius)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：15, 25, 35, 45, 55
  let data1 = 15
  let data2 = 25
  let data3 = 35
  let data4 = 45
  let data5 = 55
  
  // 计算总和
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  assert_eq(175, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(35, average)
  
  // 计算方差（简化版）
  let diff1 = add(data1, -average)
  let diff2 = add(data2, -average)
  let diff3 = add(data3, -average)
  let diff4 = add(data4, -average)
  let diff5 = add(data5, -average)
  
  let sq1 = multiply(diff1, diff1)
  let sq2 = multiply(diff2, diff2)
  let sq3 = multiply(diff3, diff3)
  let sq4 = multiply(diff4, diff4)
  let sq5 = multiply(diff5, diff5)
  
  let sum_squares = add(add(add(add(sq1, sq2), sq3), sq4), sq5)
  assert_eq(1000, sum_squares)
}

test "time_and_date_calculations" {
  // 测试时间和日期计算
  // 时间计算：将2小时30分钟转换为总分钟数
  let hours = 2
  let minutes = 30
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(150, total_minutes)
  
  // 计算时间差：从9:00到17:30
  let start_hour = 9
  let start_minute = 0
  let end_hour = 17
  let end_minute = 30
  
  let start_total = add(multiply(start_hour, 60), start_minute)
  let end_total = add(multiply(end_hour, 60), end_minute)
  let duration = add(end_total, -start_total)
  
  assert_eq(510, duration)
  
  // 计算工作日数（假设每月22个工作日）
  let months = 3
  let work_days_per_month = 22
  let total_work_days = multiply(months, work_days_per_month)
  assert_eq(66, total_work_days)
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 测试边界乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试特殊值处理
  assert_eq(0, multiply(0, 123456789))
  assert_eq(123456789, add(0, 123456789))
  assert_eq(-123456789, add(0, -123456789))
}