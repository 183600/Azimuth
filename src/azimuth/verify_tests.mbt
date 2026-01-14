// 简单测试验证脚本
// 验证 enhanced_test_scenarios.mbt 中的测试用例

// 导入必要的函数
import "azimuth"

// 测试 divide_with_ceil 函数
fn test_divide_with_ceil() {
  let result1 = divide_with_ceil(10, 4)
  println("divide_with_ceil(10, 4) = \{result1}")
  
  let result2 = divide_with_ceil(12, 4)
  println("divide_with_ceil(12, 4) = \{result2}")
  
  let result3 = divide_with_ceil(0, 5)
  println("divide_with_ceil(0, 5) = \{result3}")
  
  let result4 = divide_with_ceil(1, 1)
  println("divide_with_ceil(1, 1) = \{result4}")
}

// 测试 greet 函数
fn test_greet() {
  let result1 = greet("World")
  println("greet(\"World\") = \{result1}")
  
  let result2 = greet("")
  println("greet(\"\") = \{result2}")
  
  let result3 = greet("测试")
  println("greet(\"测试\") = \{result3}")
}

// 测试 add 和 multiply 函数
fn test_add_multiply() {
  let result1 = add(2, 3)
  println("add(2, 3) = \{result1}")
  
  let result2 = multiply(2, 3)
  println("multiply(2, 3) = \{result2}")
  
  let result3 = add(2147483647, 1)
  println("add(2147483647, 1) = \{result3}")
}

// 主函数
fn main {
  println("验证 enhanced_test_scenarios.mbt 中的测试用例...")
  test_divide_with_ceil()
  test_greet()
  test_add_multiply()
  println("验证完成！")
}