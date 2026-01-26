// 标准 MoonBit 核心测试用例
// 针对 azimuth 库的核心功能进行简洁、标准的测试

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-8, multiply(-2, 4))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "zero_operations" {
  // 测试零的运算
  assert_eq(0, multiply(0, 5))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000, add(1000, 1000))
  assert_eq(10000, multiply(100, 100))
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "practical_application" {
  // 实际应用场景：计算购物车总价
  let item_price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(100, subtotal)
  assert_eq(110, total)
}