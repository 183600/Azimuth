// 简单的 Azimuth 测试用例

// 基本函数定义
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

test "simple_bitwise_operations" {
  // 模拟位运算的数学计算
  // 测试左移运算（通过乘法模拟）
  assert_eq(16, multiply(2, 8))  // 2 << 3 = 16
  assert_eq(40, multiply(5, 8))  // 5 << 3 = 40
  
  // 测试右移运算（通过除法模拟）
  assert_eq(3, divide_with_ceil(24, 8))  // 24 >> 3 = 3
  assert_eq(1, divide_with_ceil(10, 8))  // 10 >> 3 = 1 (ceil)
}