// 新的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-3, add(-8, 5))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "add_function_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "complex_calculation" {
  // 测试复合计算
  let a = 5
  let b = 3
  let c = 2
  
  // 计算 (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(16, result)
  
  // 计算 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
}

test "string_operations" {
  // 测试字符串操作
  let name1 = "MoonBit"
  let name2 = "测试"
  let name3 = "123"
  
  assert_eq_string("Hello, MoonBit!", greet(name1))
  assert_eq_string("Hello, 测试!", greet(name2))
  assert_eq_string("Hello, 123!", greet(name3))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_scenario" {
  // 测试实际应用场景：计算订单总价
  let price = 199
  let quantity = 3
  let shipping = 15
  
  // 计算总价
  let total = add(multiply(price, quantity), shipping)
  assert_eq(612, total)
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}