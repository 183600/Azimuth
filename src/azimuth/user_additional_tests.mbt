// 用户添加的额外 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic_tests" {
  // 基本加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_tests" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(0, 100))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 各种输入的问候测试
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "add_function_edge_cases" {
  // 加法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a × b = b × a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_application_tests" {
  // 实际应用测试
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let discount = 50
  let shipping = 20
  
  // 计算总价
  let subtotal = multiply(item_price, quantity)
  let discounted_total = add(subtotal, -discount)
  let final_total = add(discounted_total, shipping)
  
  assert_eq(607, final_total)  // (199×3) - 50 + 20 = 597 - 50 + 20 = 567 + 20 = 587
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 测试特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, emoji🚀!", greet("emoji🚀"))
}

test "comprehensive_function_integration" {
  // 综合功能集成测试
  let name = "MoonBit"
  let number1 = 42
  let number2 = 24
  
  // 字符串处理
  let greeting = greet(name)
  assert_eq_string("Hello, MoonBit!", greeting)
  
  // 数值计算
  let sum = add(number1, number2)
  let product = multiply(number1, number2)
  
  assert_eq(66, sum)
  assert_eq(1008, product)
  
  // 组合计算
  let complex_result = add(multiply(number1, 2), multiply(number2, 3))
  assert_eq(156, complex_result)  // 42×2 + 24×3 = 84 + 72 = 156
}