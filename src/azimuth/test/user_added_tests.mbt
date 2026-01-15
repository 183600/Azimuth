// 用户添加的 MoonBit 测试用例
// 包含10个使用标准 MoonBit 测试语法的测试用例

test "basic_addition_operations" {
  // 测试基本加法运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_multiplication_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-4, @azimuth.multiply(-2, 2))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "string_greeting_basic" {
  // 测试基本字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "calculator_scenario" {
  // 测试计算器场景
  // 计算：(3 + 4) × 2 - 5
  let step1 = @azimuth.add(3, 4)       // 7
  let step2 = @azimuth.multiply(step1, 2)  // 14
  let result = @azimuth.add(step2, -5)     // 9
  
  @azimuth.assert_eq(9, result)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) ÷ 2
  let fahrenheit_back = 70
  let celsius_back = (@azimuth.add(fahrenheit_back, -30)) / 2
  @azimuth.assert_eq(20, celsius_back)
}

test "shopping_cart_calculation" {
  // 测试购物车计算
  let apple_price = 5
  let apple_quantity = 3
  let banana_price = 3
  let banana_quantity = 4
  
  // 计算总价
  let apple_total = @azimuth.multiply(apple_price, apple_quantity)  // 15
  let banana_total = @azimuth.multiply(banana_price, banana_quantity)  // 12
  let total = @azimuth.add(apple_total, banana_total)  // 27
  
  @azimuth.assert_eq(27, total)
}

test "area_calculations" {
  // 测试面积计算
  // 矩形面积
  let rectangle_length = 8
  let rectangle_width = 5
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  @azimuth.assert_eq(40, rectangle_area)
  
  // 三角形面积（简化版：底 × 高 ÷ 2）
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "boundary_value_tests" {
  // 测试边界值
  let zero = 0
  let one = 1
  let negative_one = -1
  
  // 零值运算
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, 100))
  @azimuth.assert_eq(100, @azimuth.add(zero, 100))
  @azimuth.assert_eq(-100, @azimuth.add(zero, -100))
  
  // 单位运算
  @azimuth.assert_eq(100, @azimuth.multiply(one, 100))
  @azimuth.assert_eq(-100, @azimuth.multiply(negative_one, 100))
}

test "boolean_assertions" {
  // 测试布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试比较操作
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(10 >= 10)
  @azimuth.assert_false(3 > 5)
  @azimuth.assert_false(10 > 10)
}