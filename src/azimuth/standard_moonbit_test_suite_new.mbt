// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(100, multiply(10, 10))
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(10, 10))
  assert_eq(100, subtract(150, 50))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))  // 10/3 = 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  assert_eq(0, divide_with_ceil(5, 0))   // 除数为0时返回0
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律
  let a = 7
  let b = 13
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_properties_identity" {
  // 测试数学单位元性质
  let x = 42
  
  // 加法单位元: x + 0 = x
  assert_eq(x, add(x, 0))
  
  // 乘法单位元: x * 1 = x
  assert_eq(x, multiply(x, 1))
  
  // 乘法零元素: x * 0 = 0
  assert_eq(0, multiply(x, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-8, 13))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 25
  let quantity = 8
  let shipping_cost = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping_cost)
  
  assert_eq(200, subtotal)  // 25 * 8 = 200
  assert_eq(215, total)     // 200 + 15 = 215
}

test "packaging_calculation_scenario" {
  // 测试包装计算场景
  // 计算需要的包装箱数量（向上取整）
  let total_items = 25
  let items_per_box = 6
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}