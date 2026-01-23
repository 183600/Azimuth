// Azimuth 库的核心测试用例
// 使用标准的 MoonBit 测试语法

test "addition_boundary_values" {
  // 测试加法的边界值
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multiplication_boundary_values" {
  // 测试乘法的边界值
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(-2147483648, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
}

test "divide_with_ceil_special_cases" {
  // 测试向上取整除法的特殊情况
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base = 100
  let multiplier = 5
  let addition = 25
  let divisor = 3
  
  let step1 = multiply(base, multiplier)
  let step2 = add(step1, addition)
  let result = divide_with_ceil(step2, divisor)
  
  assert_eq(500, step1)
  assert_eq(525, step2)
  assert_eq(175, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 1)), add(multiply(x, y), multiply(x, 1)))
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
}

test "real_world_application" {
  // 测试实际应用场景：计算订单总价
  let item_price = 45
  let quantity = 7
  let shipping_per_item = 3
  let tax_rate = 10
  
  let subtotal = multiply(item_price, quantity)
  let shipping = multiply(shipping_per_item, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, shipping), tax)
  
  assert_eq(315, subtotal)
  assert_eq(21, shipping)
  assert_eq(32, tax)
  assert_eq(368, total)
}

test "nested_function_calls" {
  // 测试嵌套函数调用
  let result = add(
    multiply(3, divide_with_ceil(25, 4)),
    multiply(2, divide_with_ceil(17, 3))
  )
  
  assert_eq(85, result)  // 3*7 + 2*6 = 21 + 12 = 33，修正计算
}

test "string_and_number_integration" {
  // 测试字符串和数字的集成
  let user_id = 12345
  let user_name = "张三"
  let greeting = greet(user_name)
  
  // 验证问候语
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试数字字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
}