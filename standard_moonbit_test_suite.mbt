// 标准 MoonBit 测试套件
// 包含对核心功能的基本测试用例

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_functionality" {
  // 测试greet函数功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 6
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculations" {
  // 测试复杂计算
  // (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum = add(a, b)
  let product = multiply(sum, c)
  let result = add(product, -d)
  
  assert_eq(22, result)
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与0和1的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "string_operations" {
  // 测试字符串操作
  let name1 = "MoonBit"
  let name2 = "123"
  let name3 = "user@example.com"
  
  assert_eq_string("Hello, MoonBit!", greet(name1))
  assert_eq_string("Hello, 123!", greet(name2))
  assert_eq_string("Hello, user@example.com!", greet(name3))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算：商品价格 × 数量 + 运费
  let price = 199
  let quantity = 2
  let shipping = 15
  
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(413, total)
}