// Azimuth 项目独立测试用例
// 包含函数定义和测试，确保独立性

// 函数定义
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

// 测试用例
test "add_function_zero_handling" {
  // 测试加法函数的零值处理
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(-5, add(0, -5))
}

test "multiply_function_negative_values" {
  // 测试乘法函数的负值处理
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, -5))
}

test "greet_function_empty_input" {
  // 测试 greet 函数的空输入处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,   !", greet("  "))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(1, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "combined_operations_calculation" {
  // 测试组合运算
  let a = 8
  let b = 3
  
  // (a + b) * 2
  let result1 = multiply(add(a, b), 2)
  assert_eq(22, result1)
  
  // a * b + a
  let result2 = add(multiply(a, b), a)
  assert_eq(32, result2)
}

test "edge_case_large_numbers" {
  // 测试大数处理的边界情况
  let large_num = 1000000
  let result1 = add(large_num, large_num)
  let result2 = multiply(1000, 1000)
  
  assert_eq(2000000, result1)
  assert_eq(1000000, result2)
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  let left = multiply(x, add(y, 3))
  let right = add(multiply(x, y), multiply(x, 3))
  assert_eq(left, right)
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边界情况
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC!", greet("ABC"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
}

test "division_by_negative_numbers" {
  // 测试负数除法
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(2, divide_with_ceil(-6, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
}

test "complex_business_calculation" {
  // 测试复杂业务计算
  let price = 199
  let quantity = 3
  let tax_rate = 10
  
  // 计算总价
  let subtotal = multiply(price, quantity)
  assert_eq(597, subtotal)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(subtotal, tax_rate)
  assert_eq(60, tax)
  
  // 计算最终总价
  let total = add(subtotal, tax)
  assert_eq(657, total)
}