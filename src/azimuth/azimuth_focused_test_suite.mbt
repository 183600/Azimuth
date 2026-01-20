// Azimuth 专注测试套件 - 包含核心功能的精选测试用例
// 使用标准 MoonBit 测试语法编写

// 重新定义断言函数以便在测试中使用
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

// 重新定义核心函数以便在测试中使用
pub fn add(a : Int, b : Int) -> Int {
  if a == 0 {
    return b
  }
  if b == 0 {
    return a
  }

  let min_val = -2147483648
  let max_val = 2147483647

  if a == min_val {
    if b < 0 {
      return min_val
    }
    return a + b
  }
  if b == min_val {
    if a < 0 {
      return min_val
    }
    return a + b
  }

  if a > 0 && b > 0 {
    if a > max_val - b {
      return max_val
    }
  }

  if a < 0 && b < 0 {
    if a < min_val - b {
      return min_val
    }
  }

  return a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  let min_val = -2147483648
  let max_val = 2147483647

  if a == 0 || b == 0 {
    return 0
  }

  if a == 1 {
    return b
  }
  if b == 1 {
    return a
  }

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

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(dividend : Int, divisor : Int) -> Int {
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  if remainder > 0 { add(quotient, 1) } else { quotient }
}

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(-3, divide_with_ceil(-10, -3))
}

test "divide_with_ceil_zero_dividend" {
  // 测试被除数为0的情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -3))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算场景：贷款分期计算
  let loan_amount = 10000
  let interest_rate = 5  // 5%
  let months = 12
  
  // 计算月利率（简化为整数计算）
  let monthly_rate = interest_rate / 12
  // 计算总利息（简化版）
  let total_interest = multiply(loan_amount, multiply(monthly_rate, months)) / 100
  // 计算每月还款（向上取整）
  let monthly_payment = divide_with_ceil(add(loan_amount, total_interest), months)
  
  assert_eq(875, monthly_payment)
}

test "geometric_progression_sum" {
  // 测试等比数列求和：1 + 2 + 4 + 8 + 16 = 31
  let first_term = 1
  let ratio = 2
  
  // 手动计算等比数列和
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(31, sum)
}

test "string_processing_with_numbers" {
  // 测试字符串与数字混合处理
  let user_id = "12345"
  let username = "john_doe"
  let combined = username + "_" + user_id
  
  let greeting = greet(combined)
  assert_eq_string("Hello, john_doe_12345!", greeting)
  
  // 测试包含数字的字符串
  let numeric_name = "User2023"
  let numeric_greeting = greet(numeric_name)
  assert_eq_string("Hello, User2023!", numeric_greeting)
}

test "boundary_value_stress_test" {
  // 测试边界值的压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值附近的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  assert_eq(2147483646, multiply(max_val, 1))
  assert_eq(-2147483647, multiply(min_val, 1))
  
  // 测试极值与中等值的运算
  assert_eq(2147483647, add(2147483640, 10))
  assert_eq(-2147483648, add(-2147483640, -10))
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟：O(n²) 操作
  let n = 10
  
  // 模拟嵌套循环的总操作数：n × n
  let total_operations = multiply(n, n)
  assert_eq(100, total_operations)
  
  // 模拟 O(n log n) 操作（简化为 n × log₂(n)，使用整数近似）
  let log_approx = 3  // log₂(10) ≈ 3.32，取整数部分
  let n_log_n_operations = multiply(n, log_approx)
  assert_eq(30, n_log_n_operations)
}

test "real_world_inventory_management" {
  // 测试实际库存管理场景
  let item_price = 50
  let initial_stock = 100
  let sold_items = 30
  let restocked_items = 20
  
  // 计算剩余库存
  let remaining_stock = add(initial_stock, -sold_items)
  // 计算更新后的库存
  let updated_stock = add(remaining_stock, restocked_items)
  // 计算库存价值
  let inventory_value = multiply(updated_stock, item_price)
  
  assert_eq(90, updated_stock)
  assert_eq(4500, inventory_value)
}

test "temperature_conversion_precision" {
  // 测试温度转换的精确计算
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数运算：F = (C × 9 + 160) / 5，然后向上取整
  
  let celsius = 25
  let fahrenheit_numerator = add(multiply(celsius, 9), 160)
  let fahrenheit = divide_with_ceil(fahrenheit_numerator, 5)
  
  assert_eq(77, fahrenheit)  // (25×9+160)/5 = 385/5 = 77
  
  // 测试负温度
  let negative_celsius = -10
  let negative_fahrenheit_numerator = add(multiply(negative_celsius, 9), 160)
  let negative_fahrenheit = divide_with_ceil(negative_fahrenheit_numerator, 5)
  
  assert_eq(14, negative_fahrenheit)  // (-10×9+160)/5 = 70/5 = 14
}