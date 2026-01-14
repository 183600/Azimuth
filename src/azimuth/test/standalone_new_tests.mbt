// 独立的 MoonBit 测试文件
// 包含函数定义和10个测试用例

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

// 测试辅助函数，提供断言功能
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (expected == actual) {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (expected == actual) {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (condition) {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (condition) {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  } else {
    ()
  }
}

// 测试用例
test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-3, 6))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试零值运算
  assert_eq(0, multiply(0, max_val))
  assert_eq(max_val, add(0, max_val))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, 5)),
    add(multiply(a, b), multiply(a, 5))
  )
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 15
  let width = 10
  let perimeter = multiply(2, add(length, width))
  assert_eq(50, perimeter)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 12
  let height = 8
  let area = multiply(base, height) / 2
  assert_eq(48, area)
}

test "financial_calculations" {
  // 测试金融计算
  // 简单利息计算：本金 + (本金 × 利率 × 时间)
  let principal = 10000
  let rate = 5  // 5%
  let time = 3   // 3年
  let interest = multiply(principal, multiply(rate, time))
  let total = add(principal, interest)
  assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
  
  // 折扣计算：原价 - (原价 × 折扣率)
  let original_price = 200
  let discount_rate = 15  // 15%
  let discount = multiply(original_price, discount_rate) / 100
  let final_price = add(original_price, -discount)
  assert_eq(170, final_price)
}

test "temperature_conversion" {
  // 测试温度转换
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) ÷ 2
  let fahrenheit_back = 80
  let celsius_back = add(fahrenheit_back, -30) / 2
  assert_eq(25, celsius_back)
  
  // 测试冰点和沸点
  assert_eq(30, add(multiply(0, 2), 30))  // 0°C = 30°F
  assert_eq(230, add(multiply(100, 2), 30))  // 100°C = 230°F
}

test "assertion_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 10
  let y = 20
  assert_true(x < y)
  assert_false(x > y)
  assert_eq(add(x, y), 30)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_true(add(max_val, 1000) == max_val)
  assert_true(add(min_val, -1000) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -2) == min_val)
  
  // 测试特殊值组合
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 99
  let item1_quantity = 2
  let item2_price = 149
  let item2_quantity = 1
  let shipping = 10
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  let final_total = add(subtotal, shipping)
  
  assert_eq(357, final_total)  // 99×2 + 149×1 + 10 = 198 + 149 + 10 = 357
  
  // 平均分计算
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let score4 = 92
  let score5 = 88
  
  let sum_scores = add(add(add(add(score1, score2), score3), score4), score5)
  let average_score = sum_scores / 5
  assert_eq(86, average_score)  // (85+90+78+92+88)/5 = 433/5 = 86
}