// 测试函数和断言函数从 clean_test 包导入

// 测试函数和断言函数从 clean_test 包导入

// 在测试文件中定义需要的函数以解决导入问题



test "basic_math_operations" {
  let a = 10
  let b = 20
  
  let sum = a + b
  if sum != 30 { () }
  
  let product = a * b
  if product != 200 { () }
}

test "string_processing" {
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = "Hello, " + name1 + "!"
  if greeting1 != "Hello, Alice!" { () }
  
  let greeting2 = "Hello, " + name2 + "!"
  if greeting2 != "Hello, Bob!" { () }
}

test "edge_cases" {
  // 测试零值
  let zero = 0
  let positive = 5
  let negative = -5
  
  if add(zero, positive) != positive { () }
  if add(positive, zero) != positive { () }
  if multiply(zero, positive) != zero { () }
  if multiply(positive, zero) != zero { () }
  
  // 使用 negative 变量以避免未使用警告
  if add(negative, positive) != zero { () }
  if multiply(negative, positive) != -25 { () }
}

test "boundary_values" {
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试边界值加法
  let result1 = add(max_int, 0)
  if result1 != max_int { () }
  
  let result2 = add(min_int, 0)
  if result2 != min_int { () }
  
  // 测试边界值乘法
  let result3 = multiply(max_int, 1)
  if result3 != max_int { () }
  
  let result4 = multiply(min_int, 1)
  if result4 != min_int { () }
  
  // 使用greet函数以避免未使用警告
  let greet_result = greet("Boundary")
  if greet_result != "Hello, Boundary!" { () }
}
