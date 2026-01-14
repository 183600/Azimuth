// 我们的新测试用例

test "basic_addition" {
  let result = azimuth::add(2, 3)
  azimuth::assert_eq(5, result)
}

test "basic_multiplication" {
  let result = azimuth::multiply(4, 5)
  azimuth::assert_eq(20, result)
}

test "basic_greeting" {
  let result = azimuth::greet("World")
  azimuth::assert_eq_string("Hello, World!", result)
}

test "hexadecimal_conversion" {
  let decimal_value = 255
  let high_part = azimuth::multiply(15, 16)  // 240
  let low_part = azimuth::add(decimal_value, -high_part)  // 15
  azimuth::assert_eq(240, high_part)
  azimuth::assert_eq(15, low_part)
}

test "shopping_cart" {
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  let total1 = azimuth::multiply(item1_price, item1_quantity)  // 398
  let total2 = azimuth::multiply(item2_price, item2_quantity)  // 297
  let cart_total = azimuth::add(total1, total2)  // 695
  azimuth::assert_eq(695, cart_total)
}

test "temperature_conversion" {
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  azimuth::assert_eq(80, fahrenheit)
}

test "area_calculation" {
  let length = 10
  let width = 5
  let rectangle_area = azimuth::multiply(length, width)
  azimuth::assert_eq(50, rectangle_area)
  
  let base = 8
  let height = 6
  let triangle_area = azimuth::multiply(base, height) / 2
  azimuth::assert_eq(24, triangle_area)
}

test "conditional_logic" {
  let x = 15
  let y = 20
  let condition = x < y
  let result = if condition { azimuth::add(x, y) } else { azimuth::multiply(x, y) }
  azimuth::assert_eq(35, result)
}

test "boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  let max_add_result = azimuth::add(max_val, 100)
  azimuth::assert_eq(max_val, max_add_result)
  let min_add_result = azimuth::add(min_val, -100)
  azimuth::assert_eq(min_val, min_add_result)
}

test "string_processing" {
  let name = "Alice"
  let greeting = azimuth::greet(name)
  azimuth::assert_eq_string("Hello, Alice!", greeting)
  
  let empty_greeting = azimuth::greet("")
  azimuth::assert_eq_string("Hello, !", empty_greeting)
}