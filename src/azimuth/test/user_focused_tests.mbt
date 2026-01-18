// 用户指定的 MoonBit 测试用例
// 包含函数定义和测试

// 函数定义
pub fn add(a : Int, b : Int) -> Int {
  // 特殊情况处理：如果有一个是0，直接返回另一个
  if a == 0 {
    return b
  }
  if b == 0 {
    return a
  }

  // 处理最小值的特殊情况（Int32的最小值）
  let min_val = -2147483648
  let max_val = 2147483647

  // 处理一个操作数是最小值的情况
  if a == min_val {
    // 最小值 + 负数 = 可能溢出
    if b < 0 {
      return min_val
    }
    // 最小值 + 非负数 = 不会溢出，直接计算
    return a + b
  }
  if b == min_val {
    // 负数 + 最小值 = 可能溢出
    if a < 0 {
      return min_val
    }
    // 非负数 + 最小值 = 不会溢出，直接计算
    return a + b
  }

  // 正数相加溢出检查
  if a > 0 && b > 0 {
    // 如果 a > max_val - b，则 a + b 会溢出
    if a > max_val - b {
      return max_val
    }
  }

  // 负数相加溢出检查（此时a和b都不是最小值）
  if a < 0 && b < 0 {
    // 使用减法检查：a < min_val - b
    // 由于b是负数，min_val - b = min_val + abs(b)
    if a < min_val - b {
      return min_val
    }
  }

  // 安全地进行加法运算
  return a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  // 常量定义
  let min_val = -2147483648
  let max_val = 2147483647

  // 处理0的情况
  if a == 0 || b == 0 {
    return 0
  }

  // 处理1的情况
  if a == 1 {
    return b
  }
  if b == 1 {
    return a
  }

  // 处理-1的情况（包括最小值的特殊情况）
  if a == -1 {
    // -2147483648 * -1 = 2147483648 会溢出，应该返回 min_val
    return if b == min_val { min_val } else { -b }
  }
  if b == -1 {
    // -1 * -2147483648 = 2147483648 会溢出，应该返回 min_val
    return if a == min_val { min_val } else { -a }
  }

  // 处理最小值的情况
  if a == min_val {
    // 最小值乘以任何绝对值大于1的数都会溢出
    return if b > 1 || b < -1 { min_val } else { a * b }
  }
  if b == min_val {
    // 最小值乘以任何绝对值大于1的数都会溢出
    return if a > 1 || a < -1 { min_val } else { a * b }
  }

  // 检查正负号
  let sign = if (a > 0 && b > 0) || (a < 0 && b < 0) { 1 } else { -1 }

  // 安全地计算绝对值，此时a和b都不是最小值
  let abs_a = if a < 0 { -a } else { a }
  let abs_b = if b < 0 { -b } else { b }

  // 优化的溢出检查：使用除法来避免乘法溢出
  // 检查 abs_a > max_val / abs_b 来避免乘法溢出
  if abs_a > max_val / abs_b {
    return if sign > 0 { max_val } else { min_val }
  }

  // 安全地进行乘法运算
  return a * b
}

pub fn greet(name : String) -> String {
  // 字符串拼接：空字符串会自动处理
  "Hello, " + name + "!"
}

// 测试用例
test "addition_with_various_numbers" {
  // 测试不同类型数字的加法
  let result1 = add(7, 8)
  if result1 != 15 { @builtin.abort("addition test 1 failed") }
  
  let result2 = add(123, -123)
  if result2 != 0 { @builtin.abort("addition test 2 failed") }
  
  let result3 = add(-50, -50)
  if result3 != -100 { @builtin.abort("addition test 3 failed") }
  
  let result4 = add(500, 500)
  if result4 != 1000 { @builtin.abort("addition test 4 failed") }
}

test "multiplication_table_test" {
  // 测试乘法表中的常见组合
  let result1 = multiply(6, 6)
  if result1 != 36 { @builtin.abort("multiplication test 1 failed") }
  
  let result2 = multiply(7, 7)
  if result2 != 49 { @builtin.abort("multiplication test 2 failed") }
  
  let result3 = multiply(8, 8)
  if result3 != 64 { @builtin.abort("multiplication test 3 failed") }
  
  let result4 = multiply(9, 9)
  if result4 != 81 { @builtin.abort("multiplication test 4 failed") }
}

test "greet_function_with_different_inputs" {
  // 测试不同输入的问候函数
  let result1 = greet("Alice")
  if result1 != "Hello, Alice!" { @builtin.abort("greet test 1 failed") }
  
  let result2 = greet("Bob123")
  if result2 != "Hello, Bob123!" { @builtin.abort("greet test 2 failed") }
  
  let result3 = greet("张三")
  if result3 != "Hello, 张三!" { @builtin.abort("greet test 3 failed") }
  
  let result4 = greet("user@domain.com")
  if result4 != "Hello, user@domain.com!" { @builtin.abort("greet test 4 failed") }
}

test "calculation_of_rectangle_perimeter" {
  // 测试矩形周长计算：2 × (长 + 宽)
  let length = 12
  let width = 8
  let sum = add(length, width)
  let perimeter = multiply(2, sum)
  if perimeter != 40 { @builtin.abort("rectangle perimeter test failed") }
}

test "calculation_of_discounted_price" {
  // 测试折扣价格计算：原价 - 折扣金额
  let original_price = 200
  let discount_percentage = 15
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discount_amount)
  if final_price != 170 { @builtin.abort("discounted price test failed") }
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 测试温度转换：华氏度 = 摄氏度 × 9/5 + 32（简化为整数计算）
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 9) / 5 + 32, 0)
  if fahrenheit != 68 { @builtin.abort("temperature conversion test failed") }
}

test "compound_interest_simple_calculation" {
  // 测试简单复利计算：本金 × (1 + 利率 × 年数)
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  let growth_factor = add(1, multiply(rate, years))
  let total = multiply(principal, growth_factor)
  if total != 16000 { @builtin.abort("compound interest test failed") }  // 1000 × (1 + 5×3) = 1000 × 16 = 16000
}

test "sum_of_first_n_natural_numbers" {
  // 测试前n个自然数之和：n × (n + 1) / 2
  let n = 10
  let sum = multiply(n, add(n, 1)) / 2
  if sum != 55 { @builtin.abort("sum of natural numbers test failed") }  // 10 × 11 / 2 = 55
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与正数相加的溢出保护
  let result1 = add(max_val, 100)
  if result1 != max_val { @builtin.abort("boundary test 1 failed") }
  
  // 测试最小值与负数相加的溢出保护
  let result2 = add(min_val, -100)
  if result2 != min_val { @builtin.abort("boundary test 2 failed") }
  
  // 测试最大值与-1相乘
  let result3 = multiply(max_val, -1)
  if result3 != min_val { @builtin.abort("boundary test 3 failed") }
}