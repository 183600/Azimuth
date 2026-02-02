// 标准 MoonBit 测试用例
// 针对 azimuth 库的10个核心测试用例

test "basic_addition_functionality" {
  // 测试基本加法功能
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(0, azimuth::multiply(0, 5))
  azimuth::assert_eq(-6, azimuth::multiply(-2, 3))
}

test "basic_subtraction_functionality" {
  // 测试基本减法功能
  azimuth::assert_eq(2, azimuth::subtract(5, 3))
  azimuth::assert_eq(-2, azimuth::subtract(3, 5))
  azimuth::assert_eq(0, azimuth::subtract(5, 5))
}

test "greet_function_basic" {
  // 测试基本问候功能
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
}

test "mathematical_commutative_law" {
  // 测试数学交换律
  let a = 7
  let b = 13
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
}

test "zero_operations" {
  // 测试零的运算
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, 5))
  azimuth::assert_eq(5, azimuth::add(5, 0))
  azimuth::assert_eq(5, azimuth::add(0, 5))
}

test "negative_number_operations" {
  // 测试负数运算
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
  azimuth::assert_eq(6, azimuth::multiply(-2, -3))
  azimuth::assert_eq(-6, azimuth::multiply(2, -3))
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = azimuth::add(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  azimuth::assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = azimuth::subtract(azimuth::multiply(10, 5), azimuth::divide_with_ceil(20, 3))
  azimuth::assert_eq(43, result2)  // 10*5 - ceil(20/3) = 50 - 7 = 43
}

test "practical_application_scenario" {
  // 实际应用场景：计算购物总价
  let item_price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = azimuth::multiply(item_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  
  azimuth::assert_eq(100, subtotal)
  azimuth::assert_eq(110, total)
}