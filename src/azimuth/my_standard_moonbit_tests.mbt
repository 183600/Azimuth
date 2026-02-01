// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能测试

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(7, 3))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(3, 5))
}

test "basic_arithmetic_subtraction" {
  // 基本减法运算测试
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(0, 0))
  assert_eq(-5, subtract(-2, 3))
  assert_eq(10, subtract(15, 5))
}

test "greet_functionality" {
  // 问候函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "combined_operations" {
  // 组合运算测试
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = subtract(multiply(10, 5), divide_with_ceil(20, 3))
  assert_eq(43, result2)  // 10*5 - ceil(20/3) = 50 - 7 = 43
}

test "boundary_values" {
  // 边界值测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, add(500000, 500000))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_properties" {
  // 数学性质测试
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

test "practical_application_scenario" {
  // 实际应用场景测试：计算购物总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let shipping = 10
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let items_total = add(subtotal1, subtotal2)
  let final_total = add(items_total, shipping)
  
  assert_eq(75, subtotal1)  // 25 * 3 = 75
  assert_eq(30, subtotal2)  // 15 * 2 = 30
  assert_eq(105, items_total)  // 75 + 30 = 105
  assert_eq(115, final_total)  // 105 + 10 = 115
}