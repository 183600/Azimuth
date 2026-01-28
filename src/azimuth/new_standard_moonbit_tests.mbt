// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

using @builtin { inspect }

///|
test "add_function_basic_test" {
  // 测试 add 函数的基本功能
  inspect(add(2, 3), content="5")
  inspect(add(0, 0), content="0")
  inspect(add(-2, 1), content="-1")
  inspect(add(50, 50), content="100")
}

///|
test "multiply_function_basic_test" {
  // 测试 multiply 函数的基本功能
  inspect(multiply(2, 3), content="6")
  inspect(multiply(5, 0), content="0")
  inspect(multiply(-2, 2), content="-4")
  inspect(multiply(5, 5), content="25")
}

///|
test "greet_function_basic_test" {
  // 测试 greet 函数的基本功能
  inspect(greet("World"), content="Hello, World!")
  inspect(greet("MoonBit"), content="Hello, MoonBit!")
  inspect(greet(""), content="Hello, !")
  inspect(greet("123"), content="Hello, 123!")
}

///|
test "divide_with_ceil_basic_test" {
  // 测试 divide_with_ceil 函数的基本功能
  inspect(divide_with_ceil(10, 3), content="4")
  inspect(divide_with_ceil(9, 3), content="3")
  inspect(divide_with_ceil(1, 100), content="1")
  inspect(divide_with_ceil(5, 0), content="0")
}

///|
test "combined_operations_test" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  inspect(result1, content="26")  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  inspect(result2, content="16")  // (5 + 3) * 2 = 8 * 2 = 16
}

///|
test "boundary_values_test" {
  // 测试边界值
  inspect(add(0, 1), content="1")
  inspect(add(0, -1), content="-1")
  inspect(multiply(1, 0), content="0")
  inspect(divide_with_ceil(1, 1), content="1")
  inspect(divide_with_ceil(0, 1), content="0")
}

///|
test "negative_numbers_test" {
  // 测试负数运算
  inspect(add(-2, -3), content="-5")
  inspect(multiply(-2, -3), content="6")
  inspect(multiply(2, -3), content="-6")
  inspect(divide_with_ceil(-10, 3), content="-3")
  inspect(divide_with_ceil(-10, -3), content="3")
}

///|
test "large_numbers_test" {
  // 测试大数运算
  inspect(add(1000000, 1000000), content="2000000")
  inspect(multiply(1000, 1000), content="1000000")
  inspect(divide_with_ceil(1000000, 3), content="333334")
}

///|
test "special_characters_test" {
  // 测试特殊字符和 Unicode
  inspect(greet("世界"), content="Hello, 世界!")
  inspect(greet("🚀"), content="Hello, 🚀!")
  inspect(greet("test@example.com"), content="Hello, test@example.com!")
  inspect(greet("αβγ"), content="Hello, αβγ!")
}

///|
test "practical_application_test" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let shipping = 10
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let items_total = add(subtotal1, subtotal2)
  let final_total = add(items_total, shipping)
  
  inspect(subtotal1, content="75")  // 25 * 3 = 75
  inspect(subtotal2, content="30")  // 15 * 2 = 30
  inspect(items_total, content="105")  // 75 + 30 = 105
  inspect(final_total, content="115")  // 105 + 10 = 115
}