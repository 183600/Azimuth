// 标准 MoonBit 测试套件 - 为 azimuth 库编写的高质量测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_zero_property" {
  // 测试乘法零元素性质：a * 0 = 0
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, 10))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, -5))
}

test "greet_function_with_special_characters" {
  // 测试问候函数处理特殊字符
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(6, 2))   // 3 -> 3
  assert_eq(2, divide_with_ceil(5, 3))   // 1.666... -> 2
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "combined_business_calculations" {
  // 测试组合业务计算：购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let shipping = 10
  
  let subtotal = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  let total = add(subtotal, shipping)
  
  assert_eq(105, subtotal)  // 25*3 + 15*2 = 75 + 30 = 105
  assert_eq(115, total)     // 105 + 10 = 115
}

test "packaging_efficiency_calculation" {
  // 测试包装效率计算
  let total_products = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 4 * (6 + 8) = 4 * 6 + 4 * 8 = 56
}

test "large_number_precision" {
  // 测试大数精度
  let large_num1 = 100000
  let large_num2 = 200000
  
  assert_eq(300000, add(large_num1, large_num2))
  assert_eq(20000000000, multiply(large_num1, large_num2))
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
}

test "error_handling_division_by_zero" {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}