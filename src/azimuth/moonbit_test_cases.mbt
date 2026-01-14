// MoonBit 测试用例
// 使用标准的 MoonBit 测试语法编写

// 基础数学运算测试
test "add_basic_functionality" {
  // 测试基本的加法运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

// 乘法运算测试
test "multiply_basic_functionality" {
  // 测试基本的乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 100))
}

// 字符串处理测试
test "greet_function_with_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
}

// 边界值测试
test "boundary_value_handling" {
  // 测试边界值的处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法边界值
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 测试乘法边界值
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

// 复合运算测试
test "compound_operations" {
  // 测试复合运算
  let a = 10
  let b = 20
  let c = 5
  
  // (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(150, result)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(205, result2)
}

// 实际应用场景测试
test "real_world_scenarios" {
  // 测试实际应用场景
  
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let cart_total = add(multiply(item_price, quantity), shipping)
  assert_eq(612, cart_total)
  
  // 温度转换（简化版：摄氏度转华氏度）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

// 特殊情况处理测试
test "special_cases_handling" {
  // 测试特殊情况的处理
  
  // 最小值与-1相乘
  let min_val = -2147483648
  assert_eq(min_val, multiply(min_val, -1))
  
  // 最大值与正数相加
  let max_val = 2147483647
  assert_eq(max_val, add(max_val, 100))
  
  // 零值运算
  assert_eq(0, multiply(0, min_val))
  assert_eq(max_val, add(0, max_val))
}

// 断言函数测试
test "assertion_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试比较结果
  let x = 10
  let y = 20
  assert_true(x < y)
  assert_false(x > y)
  assert_eq(add(x, y), 30)
}

// 字符串边界情况测试
test "string_edge_cases" {
  // 测试字符串的边界情况
  
  // 长字符串
  let long_string = "ThisIsAVeryLongStringThatMightCauseIssuesInSomeSystems"
  assert_eq_string("Hello, " + long_string + "!", greet(long_string))
  
  // 包含特殊字符的字符串
  let special_chars = "test@123#$%"
  assert_eq_string("Hello, test@123#$%!", greet(special_chars))
  
  // Unicode 字符串
  let unicode_string = "测试🌍"
  assert_eq_string("Hello, 测试🌍!", greet(unicode_string))
}

// 数学性质验证测试
test "mathematical_properties" {
  // 测试数学性质
  
  // 加法交换律
  let a = 15
  let b = 25
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  let c = 7
  let d = 11
  assert_eq(multiply(c, d), multiply(d, c))
  
  // 分配律
  let e = 5
  let f = 3
  let g = 2
  assert_eq(multiply(e, add(f, g)), add(multiply(e, f), multiply(e, g)))
}