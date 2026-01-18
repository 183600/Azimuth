// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法，包含断言验证

test "addition_boundary_values" {
  // 测试加法函数的边界值情况
  assert_eq(0, add(0, 0))
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multiplication_zero_properties" {
  // 测试乘法函数的零值特性
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -5))
  assert_eq(0, multiply(-5, 0))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(" "))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "compound_operations" {
  // 测试复合运算：(a + b) * (c + d)
  let a = 3
  let b = 4
  let c = 5
  let d = 6
  
  let sum1 = add(a, b)
  let sum2 = add(c, d)
  let result = multiply(sum1, sum2)
  
  assert_eq(77, result)
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 7
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "large_number_operations" {
  // 测试大数运算
  let large1 = 1000000
  let large2 = 2000000
  
  assert_eq(3000000, add(large1, large2))
  assert_eq(2000000000000, multiply(large1, large2))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(-2, add(3, -5))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
}

test "real_world_shopping_cart" {
  // 测试真实世界场景：购物车计算
  let item1_price = 25
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let tax_rate = 10
  
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  let subtotal = add(item1_total, item2_total)
  let tax = multiply(subtotal, tax_rate) / 100
  let total = add(subtotal, tax)
  
  assert_eq(110, total)
}

test "distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 5
  let c = 6
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "sequential_operations" {
  // 测试连续运算
  let initial = 10
  
  // 连续加法
  let result1 = add(add(add(initial, 5), 3), 2)
  assert_eq(20, result1)
  
  // 连续乘法
  let result2 = multiply(multiply(multiply(initial, 2), 3), 4)
  assert_eq(240, result2)
  
  // 混合运算
  let result3 = multiply(add(initial, 5), multiply(2, 3))
  assert_eq(90, result3)
}