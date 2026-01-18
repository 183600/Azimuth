// Standalone test cases for Azimuth project
// 独立测试用例，包含所有必要的函数定义

// 函数定义
fn add(a : Int, b : Int) -> Int {
  if a == 0 { return b }
  if b == 0 { return a }
  
  let min_val = -2147483648
  let max_val = 2147483647
  
  if a == min_val {
    if b < 0 { return min_val }
    return a + b
  }
  if b == min_val {
    if a < 0 { return min_val }
    return a + b
  }
  
  if a > 0 && b > 0 {
    if a > max_val - b { return max_val }
  }
  
  if a < 0 && b < 0 {
    if a < min_val - b { return min_val }
  }
  
  return a + b
}

fn multiply(a : Int, b : Int) -> Int {
  let min_val = -2147483648
  let max_val = 2147483647
  
  if a == 0 || b == 0 { return 0 }
  if a == 1 { return b }
  if b == 1 { return a }
  
  if a == -1 {
    return if b == min_val { min_val } else { -b }
  }
  if b == -1 {
    return if a == min_val { min_val } else { -a }
  }
  
  if a == min_val {
    return if b > 1 || b < -1 { min_val } else { a * b }
  }
  if b == min_val {
    return if a > 1 || a < -1 { min_val } else { a * b }
  }
  
  let sign = if (a > 0 && b > 0) || (a < 0 && b < 0) { 1 } else { -1 }
  let abs_a = if a < 0 { -a } else { a }
  let abs_b = if b < 0 { -b } else { b }
  
  if abs_a > max_val / abs_b {
    return if sign > 0 { max_val } else { min_val }
  }
  
  return a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 断言函数
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

fn assert_true(condition : Bool) -> Unit {
  if (condition) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    let _ = 1 / 0
    ()
  } else {
    ()
  }
}

// 测试用例
test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  let f0 = 0
  let f1 = 1
  let f2 = add(f0, f1)  // 1
  let f3 = add(f1, f2)  // 2
  let f4 = add(f2, f3)  // 3
  let f5 = add(f3, f4)  // 5
  let f6 = add(f4, f5)  // 8
  let f7 = add(f5, f6)  // 13
  let f8 = add(f6, f7)  // 21
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
}

test "prime_number_validation" {
  // 质数验证测试
  let num = 7
  let mut is_prime = true
  
  if (num % 2 == 0) {
    is_prime = false
  }
  
  if (num % 3 == 0) {
    is_prime = false
  }
  
  assert_true(is_prime)
  
  let num2 = 9
  let mut is_prime2 = true
  
  if (num2 % 2 == 0) {
    is_prime2 = false
  }
  
  if (num2 % 3 == 0) {
    is_prime2 = false
  }
  
  assert_false(is_prime2)
}

test "bank_interest_calculation" {
  // 银行利息计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  let interest = multiply(principal, multiply(rate, years)) / 100
  let total = add(principal, interest)
  
  assert_eq(1500, interest)
  assert_eq(11500, total)
  
  let compound_total = multiply(principal, add(100, multiply(rate, years))) / 100
  assert_eq(11500, compound_total)
}

test "temperature_conversion_complete" {
  // 完整的温度转换测试
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  assert_eq(77, fahrenheit)
  
  let freezing_c = 0
  let freezing_f = add(multiply(freezing_c, 9), 160) / 5
  assert_eq(32, freezing_f)
  
  let boiling_c = 100
  let boiling_f = add(multiply(boiling_c, 9), 160) / 5
  assert_eq(212, boiling_f)
}

test "speed_distance_time_calculation" {
  // 速度、距离、时间计算测试
  let speed = 60
  let time = 2
  let distance = multiply(speed, time)
  
  assert_eq(120, distance)
  
  let total_distance = 300
  let total_time = 5
  let average_speed = total_distance / total_time
  
  assert_eq(60, average_speed)
  
  let planned_distance = 180
  let planned_speed = 90
  let travel_time = planned_distance / planned_speed
  
  assert_eq(2, travel_time)
}

test "geometric_progression_sum" {
  // 等比数列求和测试
  let first_term = 2
  let ratio = 3
  
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let sum = add(add(add(term1, term2), term3), term4)
  
  assert_eq(2, term1)
  assert_eq(6, term2)
  assert_eq(18, term3)
  assert_eq(54, term4)
  assert_eq(80, sum)
}

test "discount_price_calculation" {
  // 折扣价格计算测试
  let original_price = 200
  let discount_percent = 15
  
  let discount_amount = multiply(original_price, discount_percent) / 100
  let discounted_price = add(original_price, -discount_amount)
  
  assert_eq(30, discount_amount)
  assert_eq(170, discounted_price)
  
  let price = 500
  let first_discount = 10
  let second_discount = 5
  
  let first_amount = multiply(price, first_discount) / 100
  let after_first = add(price, -first_amount)
  let second_amount = multiply(after_first, second_discount) / 100
  let final_price = add(after_first, -second_amount)
  
  assert_eq(50, first_amount)
  assert_eq(450, after_first)
  assert_eq(22, second_amount)
  assert_eq(428, final_price)
}

test "array_manipulation_simulation" {
  // 数组操作模拟测试
  let sum = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum)
  
  let a = 10
  let b = 25
  let c = 15
  let d = 30
  let e = 20
  
  let max1 = if (a > b) { a } else { b }
  let max2 = if (max1 > c) { max1 } else { c }
  let max3 = if (max2 > d) { max2 } else { d }
  let max4 = if (max3 > e) { max3 } else { e }
  
  assert_eq(30, max4)
  
  let min1 = if (a < b) { a } else { b }
  let min2 = if (min1 < c) { min1 } else { c }
  let min3 = if (min2 < d) { min2 } else { d }
  let min4 = if (min3 < e) { min3 } else { e }
  
  assert_eq(10, min4)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  let mixed_string = "Hello123世界!@#"
  let result = greet(mixed_string)
  assert_eq_string("Hello, Hello123世界!@#!", result)
  
  let name1 = "Alice"
  let name2 = "Bob"
  let separator = " and "
  
  let combined_name = name1 + separator + name2
  let combined_greeting = greet(combined_name)
  
  assert_eq_string("Hello, Alice and Bob!", combined_greeting)
  
  let spaced_name = "  John  Doe  "
  let trimmed_name = "John  Doe"
  let trimmed_greeting = greet(trimmed_name)
  
  assert_eq_string("Hello, John  Doe!", trimmed_greeting)
}

test "complex_business_logic" {
  // 复杂业务逻辑测试
  let base_salary = 5000
  let overtime_hours = 10
  let overtime_rate = 50
  let bonus = 1000
  let tax_rate = 10
  
  let overtime_pay = multiply(overtime_hours, overtime_rate)
  let gross_income = add(add(base_salary, overtime_pay), bonus)
  let tax_amount = multiply(gross_income, tax_rate) / 100
  let net_income = add(gross_income, -tax_amount)
  
  assert_eq(500, overtime_pay)
  assert_eq(6500, gross_income)
  assert_eq(650, tax_amount)
  assert_eq(5850, net_income)
  
  let material_cost = 10000
  let labor_cost = 8000
  let overhead_rate = 15
  let profit_margin = 20
  
  let direct_cost = add(material_cost, labor_cost)
  let overhead_cost = multiply(direct_cost, overhead_rate) / 100
  let total_cost = add(direct_cost, overhead_cost)
  let selling_price = multiply(total_cost, add(100, profit_margin)) / 100
  
  assert_eq(18000, direct_cost)
  assert_eq(2700, overhead_cost)
  assert_eq(20700, total_cost)
  assert_eq(24840, selling_price)
}