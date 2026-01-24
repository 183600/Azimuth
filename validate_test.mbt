// 验证测试文件
// 导入 azimuth 库
use azimuth

// 测试 add 函数
fn test_add() {
  let result = azimuth.add(2, 3)
  if result == 5 {
    "add test passed"
  } else {
    "add test failed"
  }
}

// 测试 multiply 函数
fn test_multiply() {
  let result = azimuth.multiply(3, 4)
  if result == 12 {
    "multiply test passed"
  } else {
    "multiply test failed"
  }
}

// 测试 greet 函数
fn test_greet() {
  let result = azimuth.greet("World")
  if result == "Hello, World!" {
    "greet test passed"
  } else {
    "greet test failed"
  }
}

// 测试 divide_with_ceil 函数
fn test_divide_with_ceil() {
  let result = azimuth.divide_with_ceil(10, 3)
  if result == 4 {
    "divide_with_ceil test passed"
  } else {
    "divide_with_ceil test failed"
  }
}

// 主函数
fn main() {
  println(test_add())
  println(test_multiply())
  println(test_greet())
  println(test_divide_with_ceil())
}