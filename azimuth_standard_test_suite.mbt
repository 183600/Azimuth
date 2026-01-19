// 标准MoonBit测试用例集合
// 这10个测试用例展示了MoonBit的标准测试语法和常见测试模式

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "add_boundary_values" {
  // 测试边界值的加法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试最小值溢出保护
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试混合边界值
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_special_cases" {
  // 测试乘法的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试1和-1的特殊情况
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(5, -1))
  assert_eq(-5, multiply(-1, 5))
  
  // 测试最小值的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
}

test "greet_function_tests" {
  // 测试greet函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
  
  // 场景2：计算购物车总价
  let price = 199
  let quantity = 3
  let shipping = 20
  let total = add(multiply(price, quantity), shipping)
  assert_eq(617, total)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  let c = 3
  
  // 测试加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 测试乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(add(min_val, -100) == min_val)
}

test "real_world_applications" {
  // 测试实际应用场景
  // 场景1：温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 场景2：简单贷款计算
  let principal = 10000
  let rate = 5
  let years = 3
  let total = multiply(principal, add(1, multiply(rate, years)))
  assert_eq(160000, total)
}

test "performance_and_stress_tests" {
  // 测试性能和压力场景
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(2000000000000, multiply(large_num1, large_num2))
  
  // 测试连续运算
  let result = add(multiply(10, 20), multiply(30, 40))
  assert_eq(1400, result)
  
  // 测试嵌套运算
  let nested = multiply(add(5, 10), add(3, 7))
  assert_eq(150, nested)
}