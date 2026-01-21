// Azimuth 标准测试套件 - 简洁全面的测试用例

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 交换律
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
}

test "business_calculation" {
  // 业务计算测试
  let price = 25
  let quantity = 4
  let shipping = 10
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(110, total)
}

test "resource_planning" {
  // 资源规划测试
  let total_tasks = 37
  let tasks_per_day = 8
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(5, days_needed)
}

test "complex_calculation" {
  // 复杂计算测试
  let result = add(
    multiply(5, divide_with_ceil(17, 4)),
    multiply(3, divide_with_ceil(25, 6))
  )
  
  assert_eq(34, result)
}

test "error_handling" {
  // 错误处理测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_processing" {
  // 字符串处理测试
  let name = "MoonBit"
  let greeting = greet(name)
  let expected = "Hello, " + name + "!"
  
  assert_eq_string(expected, greeting)
}