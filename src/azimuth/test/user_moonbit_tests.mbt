// 用户自定义的 MoonBit 测试用例
// 测试项目的核心功能：add、multiply 和 greet 函数

test "add_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-3, 3))
  assert_eq(-5, add(-2, -3))
}

test "multiply_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
}

test "greet_basic_functionality" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
}

test "add_boundary_values" {
  // 测试 add 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试最小值溢出保护
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_boundary_values" {
  // 测试 multiply 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  
  // 测试最小值特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_edge_cases" {
  // 测试 greet 函数的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 特殊字符!", greet("特殊字符"))
}

test "complex_calculations" {
  // 测试复合计算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(16, result)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 4
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 2)), add(multiply(a, b), multiply(a, 2)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形周长：2 * (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
  
  // 计算商品总价：单价 * 数量
  let price = 199
  let quantity = 3
  let total = multiply(price, quantity)
  assert_eq(597, total)
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}