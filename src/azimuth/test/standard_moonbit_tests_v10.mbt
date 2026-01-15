// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(0, add(0, 0))
  
  // 测试边界值
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 100))
  assert_eq(100, multiply(10, 10))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  assert_eq(-5, multiply(5, -1))
  assert_eq(5, multiply(-5, -1))
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  
  // 测试边界值
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
}

test "greet_function_standard_tests" {
  // 测试 greet 函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户!", greet("用户"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(40, area)
  assert_eq(26, perimeter)
  
  // 计算平均值
  let scores = add(add(85, 90), 95)
  let average = scores / 3
  assert_eq(90, average)
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 7
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "real_world_application_tests" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let discount = 50
  let shipping = 20
  
  let subtotal = multiply(item_price, quantity)
  let total = add(add(subtotal, -discount), shipping)
  
  assert_eq(607, total)
  
  // 温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A B C!", greet("A B C"))
  
  // 测试Unicode字符
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 中文English123!", greet("中文English123"))
}

test "performance_and_stress_tests" {
  // 测试性能和压力情况
  // 大数运算
  let big_num1 = 1000000
  let big_num2 = 2000000
  
  assert_eq(3000000, add(big_num1, big_num2))
  assert_eq(2000000000000, multiply(big_num1, big_num2))
  
  // 连续运算
  let result = add(multiply(100, 20), multiply(50, 30))
  assert_eq(3500, result)
  
  // 嵌套运算
  let nested = multiply(add(10, 20), add(5, 15))
  assert_eq(600, nested)
}