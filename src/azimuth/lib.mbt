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

///
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

///
pub fn greet(name : String) -> String {
  // 字符串拼接：空字符串会自动处理
  "Hello, " + name + "!"
}

// 测试辅助函数，提供断言功能
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  // 简单的断言实现，不依赖外部包
  if expected != actual {
    // 在实际环境中，这里会触发断言失败
    @builtin.panic()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  // 简单的断言实现，不依赖外部包
  if expected != actual {
    // 在实际环境中，这里会触发断言失败
    @builtin.panic()
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  // 简单的断言实现，不依赖外部包
  if !condition {
    // 在实际环境中，这里会触发断言失败
    @builtin.panic()
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  // 简单的断言实现，不依赖外部包
  if condition {
    // 在实际环境中，这里会触发断言失败
    @builtin.panic()
  }
}


