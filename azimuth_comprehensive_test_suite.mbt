// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法，包含断言和 Unit 返回类型测试

test "add_function_basic" {
  // 测试 add 函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_basic" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_business_calculation" {
  // 测试复杂商业计算：总价计算（含税和折扣）
  let price = 100
  let quantity = 3
  let discount = 50
  let tax_rate = 10
  
  let subtotal = multiply(price, quantity)
  let after_discount = subtotal - discount
  let tax_amount = (after_discount * tax_rate) / 100
  let total = add(after_discount, tax_amount)
  
  assert_eq(280, total)
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  let square_side = 5
  let rect_length = 8
  let rect_width = 4
  let triangle_base = 6
  let triangle_height = 4
  
  let square_area = multiply(square_side, square_side)
  let rect_area = multiply(rect_length, rect_width)
  let triangle_area = (triangle_base * triangle_height) / 2
  
  assert_eq(25, square_area)
  assert_eq(32, rect_area)
  assert_eq(12, triangle_area)
}

test "temperature_conversion" {
  // 测试温度转换计算
  let celsius = 25
  let fahrenheit = add(((celsius * 9) / 5), 32)
  let kelvin = add(celsius, 273)
  
  assert_eq(77, fahrenheit)
  assert_eq(298, kelvin)
}

test "statistical_calculations" {
  // 测试统计计算
  let numbers = [10, 20, 30, 40, 50]
  let sum = add(add(add(add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  let mean = sum / 5
  
  assert_eq(150, sum)
  assert_eq(30, mean)
}

test "unit_conversions" {
  // 测试单位转换
  let kilometers = 5
  let meters = multiply(kilometers, 1000)
  let centimeters = multiply(meters, 100)
  
  let kilograms = 2
  let grams = multiply(kilograms, 1000)
  
  assert_eq(5000, meters)
  assert_eq(500000, centimeters)
  assert_eq(2000, grams)
}

test "compound_interest" {
  // 测试复利计算
  let principal = 1000
  let rate = 5
  let years = 3
  
  let year1 = add(principal, (principal * rate) / 100)
  let year2 = add(year1, (year1 * rate) / 100)
  let year3 = add(year2, (year2 * rate) / 100)
  
  assert_eq(1158, year3)
}

test "pattern_calculations" {
  // 测试模式计算（斐波那契数列前几项）
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
}