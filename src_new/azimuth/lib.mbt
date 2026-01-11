// 基础库文件
pub fn add(a : Int, b : Int) -> Int {
  // 特殊情况处理：如果有一个是0，直接返回另一个
  if a == 0 { return b }
  if b == 0 { return a }
  
  // 正数相加溢出检查
  if a > 0 && b > 0 {
    // 如果 a > 2147483647 - b，则 a + b 会溢出
    if a > 2147483647 - b { return 2147483647 }
  }
  
  // 负数相加溢出检查
  if a < 0 && b < 0 {
    // 特殊情况处理：如果任一参数是最小值
    if a == -2147483648 || b == -2147483648 {
      // 最小值加任何负数都会溢出
      return -2147483648
    }
    
    // 对于其他负数，使用更安全的溢出检查
    // 检查 a + b 是否会小于最小值
    // 使用更安全的比较：如果 b < -2147483648 - a，则会溢出
    // 这样可以避免在边界情况下的计算错误
    let threshold = -2147483648 - a
    if b < threshold { 
      return -2147483648 
    }
  }
  
  // 安全地进行加法运算
  return a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  // 处理0的情况
  if a == 0 || b == 0 { return 0 }
  
  // 处理1的情况
  if a == 1 { return b }
  if b == 1 { return a }
  
  // 优化：统一处理-1的情况（包括最小值的特殊情况）
  if a == -1 {
    return if b == -2147483648 { -2147483648 } else { -b }
  }
  if b == -1 {
    return if a == -2147483648 { -2147483648 } else { -a }
  }
  
  // 优化：统一处理最小值的情况
  let min_val = -2147483648
  if a == min_val || b == min_val {
    let other = if a == min_val { b } else { a }
    // 最小值乘以任何绝对值大于1的数都会溢出
    if other > 1 || other < -1 { return min_val }
    // 其他情况（0, 1, -1）已经在上面的处理中覆盖
  }
  
  // 检查正负号
  let sign = if (a > 0 && b > 0) || (a < 0 && b < 0) { 1 } else { -1 }
  
  // 安全地计算绝对值，此时a和b都不是最小值
  let abs_a = if a < 0 { -a } else { a }
  let abs_b = if b < 0 { -b } else { b }
  
  // 优化的溢出检查：使用除法来避免乘法溢出
  // 检查 abs_a > 2147483647 / abs_b 来避免乘法溢出
  // 注意：abs_b > 0 的检查防止除以0
  if abs_b > 0 && abs_a > 2147483647 / abs_b {
    return if sign > 0 { 2147483647 } else { -2147483648 }
  }
  
  // 安全地进行乘法运算
  return a * b
}

pub fn greet(name : String) -> String {
  // 字符串拼接：空字符串会自动处理
  "Hello, " + name + "!"
}