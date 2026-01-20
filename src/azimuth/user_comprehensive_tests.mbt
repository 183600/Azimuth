// 用户综合测试用例 - 测试核心功能的各种场景
// 使用标准 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
  assert_eq(100, add(50, 50))
}

test "add_function_overflow_protection" {
  // 测试加法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 负溢出测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(min_val, add(-1000000000, -1500000000))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(25, multiply(5, 5))
}

test "multiply_function_overflow_protection" {
  // 测试乘法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正溢出测试
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(max_val, multiply(46341, 46341))
  
  // 负溢出测试
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 特殊情况：最小值乘以-1
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_comprehensive" {
  // 测试greet函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 7
  let b = 11
  let c = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 结合律测试
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "real_world_calculation_scenarios" {
  // 测试实际应用场景
  // 场景1：计算矩形周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
  
  // 场景2：购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let total = add(multiply(item_price, quantity), shipping)
  assert_eq(617, total)
  
  // 场景3：温度转换（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值组合
  assert_eq(-1, add(max_val, min_val))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  
  // 1和-1的特殊处理
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(5, multiply(-5, -1))
  
  // 连续运算测试
  let result = add(multiply(10, 5), multiply(3, 2))
  assert_eq(56, result)
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  // 计算：((a + b) * c - d) / e + f
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  let e = 5
  let f = 8
  
  let sum_ab = add(a, b)        // 30
  let product = multiply(sum_ab, c)  // 90
  let difference = add(product, -d)  // 75
  let quotient = difference / e      // 15
  let final_result = add(quotient, f)  // 23
  
  assert_eq(23, final_result)
}

test "assertion_functions_validation" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试比较结果
  assert_true(add(2, 3) == 5)
  assert_false(add(2, 3) == 6)
  assert_true(multiply(4, 5) > 15)
  assert_false(multiply(4, 5) < 15)
  
  // 测试字符串比较
  assert_true(greet("Test") == "Hello, Test!")
  assert_false(greet("Test") == "Hello, test!")
}