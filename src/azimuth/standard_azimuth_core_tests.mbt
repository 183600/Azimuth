// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含基本功能测试、边界情况和实际应用场景

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_commutative_property" {
  // 测试乘法函数的交换律性质
  assert_eq(15, multiply(3, 5))
  assert_eq(15, multiply(5, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "divide_with_ceil_positive_division" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "real_world_shopping_cart_calculation" {
  // 实际应用场景：购物车计算
  let item_price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(100, subtotal)
  assert_eq(110, total)
}

test "packaging_efficiency_calculation" {
  // 实际应用场景：包装效率计算
  let total_products = 47
  let capacity_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_products, capacity_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法单位元
  assert_eq(7, add(7, 0))
  assert_eq(-3, add(-3, 0))
  
  // 乘法单位元
  assert_eq(8, multiply(8, 1))
  assert_eq(-5, multiply(-5, 1))
  
  // 乘法零元素
  assert_eq(0, multiply(15, 0))
  assert_eq(0, multiply(0, 100))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}