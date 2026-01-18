// 额外的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能和边界情况

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

// 辅助断言函数
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

pub fn assert_true(condition : Bool) -> Unit {
  if (condition) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    let _ = 1 / 0
    ()
  } else {
    ()
  }
}

test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 123
  let b = 456
  
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let c = -789
  let d = 321
  
  assert_eq(add(c, d), add(d, c))
}

test "multiply_function_associativity" {
  // 测试乘法结合律：(a * b) * c = a * (b * c)
  let a = 2
  let b = 3
  let c = 4
  
  let result1 = multiply(multiply(a, b), c)
  let result2 = multiply(a, multiply(b, c))
  
  assert_eq(result1, result2)
}

test "mixed_operations" {
  // 测试混合运算
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(45, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(53, result2)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与1的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 边界值与-1的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_with_unicode" {
  // 测试包含 Unicode 字符的字符串
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🎉!", greet("🎉"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let years = 3          // 年数
  
  // 简单利息：本金 + (本金 * 利率 * 年数)
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + (10000 * 5 * 3) = 160000
  
  // 复利计算（简化版）
  let compound_total = multiply(principal, add(1, multiply(rate, years)))
  assert_eq(160000, compound_total)
}

test "geometry_calculations" {
  // 测试几何计算
  let radius = 5
  
  // 圆周长（简化版）：2 * π * r ≈ 2 * 3 * r
  let circumference = multiply(multiply(2, 3), radius)
  assert_eq(30, circumference)
  
  // 圆面积（简化版）：π * r² ≈ 3 * r²
  let area = multiply(3, multiply(radius, radius))
  assert_eq(75, area)
  
  // 球体体积（简化版）：4/3 * π * r³ ≈ 4 * r³
  let volume = multiply(4, multiply(radius, multiply(radius, radius)))
  assert_eq(500, volume)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C * 9/5 + 32，简化为 F = C * 2 + 30
  
  // 冰点：0°C = 30°F（简化版）
  let freezing_c = 0
  let freezing_f = add(multiply(freezing_c, 2), 30)
  assert_eq(30, freezing_f)
  
  // 沸点：100°C = 230°F（简化版）
  let boiling_c = 100
  let boiling_f = add(multiply(boiling_c, 2), 30)
  assert_eq(230, boiling_f)
  
  // 室温：25°C = 80°F（简化版）
  let room_c = 25
  let room_f = add(multiply(room_c, 2), 30)
  assert_eq(80, room_f)
}

test "array_index_calculation" {
  // 测试数组索引计算
  let row = 3
  let col = 4
  let width = 10
  
  // 二维数组转一维数组的索引：index = row * width + col
  let index = add(multiply(row, width), col)
  assert_eq(34, index)
  
  // 测试边界情况
  let first_index = add(multiply(0, width), 0)
  assert_eq(0, first_index)
  
  let last_index = add(multiply(9, width), 9)
  assert_eq(99, last_index)
}

test "error_handling" {
  // 测试错误处理和边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}