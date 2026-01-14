// 使用内置断言的测试
// 测试基本功能

test "simple_addition" {
  // 测试基本加法
  let result = 2 + 3
  if result != 5 { panic("addition test failed") }
}

test "simple_multiplication" {
  // 测试基本乘法
  let result = 2 * 3
  if result != 6 { panic("multiplication test failed") }
}

test "simple_string" {
  // 测试字符串拼接
  let result = "Hello" + ", " + "World"
  if result != "Hello, World" { panic("string test failed") }
}

test "azimuth_functions" {
  // 测试 azimuth 包的函数
  let add_result = add(2, 3)
  if add_result != 5 { panic("add function test failed") }
  
  let multiply_result = multiply(2, 3)
  if multiply_result != 6 { panic("multiply function test failed") }
  
  let greet_result = greet("World")
  if greet_result != "Hello, World!" { panic("greet function test failed") }
}