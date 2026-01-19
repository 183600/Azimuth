// 标准 MoonBit 测试套件
// 包含基础功能测试用例

test "addition_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
}

test "multiplication_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(21, multiply(7, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(8, 0))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 用户!", greet("用户"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base_price = 100
  let quantity = 5
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(base_price, quantity)
  let tax = multiply(subtotal, tax_rate) / 100
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(555, total)
}

test "string_special_characters" {
  // 测试特殊字符处理
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, C++ Developer!", greet("C++ Developer"))
  assert_eq_string("Hello, $100!", greet("$100"))
}

test "assertion_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 15
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  
  // 乘法分配律
  assert_eq(multiply(x, 5), add(multiply(x, 2), multiply(x, 3)))
}

test "real_world_application" {
  // 测试实际应用场景：计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(96, area)
  assert_eq(40, perimeter)
}