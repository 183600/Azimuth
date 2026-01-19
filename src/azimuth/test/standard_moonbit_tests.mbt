// 标准的 MoonBit 测试用例
// 覆盖 azimuth 项目的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  let result1 = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result1)
  
  let result2 = @azimuth.add(-2, 3)
  @azimuth.assert_eq(1, result2)
  
  let result3 = @azimuth.add(0, 0)
  @azimuth.assert_eq(0, result3)
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  let result1 = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result1)
  
  let result2 = @azimuth.multiply(-2, 3)
  @azimuth.assert_eq(-6, result2)
  
  let result3 = @azimuth.multiply(0, 5)
  @azimuth.assert_eq(0, result3)
}

test "greet_functionality" {
  // 测试问候功能
  let result1 = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result1)
  
  let result2 = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result2)
  
  let result3 = @azimuth.greet("测试")
  @azimuth.assert_eq_string("Hello, 测试!", result3)
}

test "add_with_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正常边界值
  let result1 = @azimuth.add(max_val, 0)
  @azimuth.assert_eq(max_val, result1)
  
  let result2 = @azimuth.add(min_val, 0)
  @azimuth.assert_eq(min_val, result2)
  
  // 测试溢出保护
  let result3 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result3)
  
  let result4 = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result4)
}

test "multiply_with_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0相乘
  let result1 = @azimuth.multiply(max_val, 0)
  @azimuth.assert_eq(0, result1)
  
  let result2 = @azimuth.multiply(min_val, 0)
  @azimuth.assert_eq(0, result2)
  
  // 测试与1相乘
  let result3 = @azimuth.multiply(max_val, 1)
  @azimuth.assert_eq(max_val, result3)
  
  let result4 = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_val, result4)
  
  // 测试最小值与-1相乘的特殊情况
  let result5 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result5)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let length = 10
  let width = 5
  
  // 计算矩形周长：2 * length + 2 * width
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
  
  // 计算矩形面积：length * width
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 3
  
  // 加法交换律
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(b, a)
  @azimuth.assert_eq(sum1, sum2)
  
  // 乘法交换律
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(product1, product2)
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 5
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "string_operations" {
  // 测试字符串操作
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "测试用户"
  
  // 测试不同名称的问候
  let greeting1 = @azimuth.greet(name1)
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  
  let greeting2 = @azimuth.greet(name2)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  
  let greeting3 = @azimuth.greet(name3)
  @azimuth.assert_eq_string("Hello, 测试用户!", greeting3)
}

test "sequential_operations" {
  // 测试连续运算
  let a = 5
  let b = 10
  let c = 15
  
  // 连续加法：(a + b) + c
  let sum1 = @azimuth.add(@azimuth.add(a, b), c)
  @azimuth.assert_eq(30, sum1)
  
  // 连续乘法：a * b * c
  let product1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(750, product1)
  
  // 混合运算：a + b * c
  let mixed1 = @azimuth.add(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(155, mixed1)
  
  // 另一种混合运算：(a + b) * c
  let mixed2 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(225, mixed2)
}

test "practical_application" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算最终总价（含运费）
  let total = @azimuth.add(subtotal, shipping_fee)
  
  // 验证结果：199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
  @azimuth.assert_eq(717, total)
}