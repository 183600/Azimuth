// 额外的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 1))  // 正溢出
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -1))  // 负溢出
  @azimuth.assert_eq(-1, @azimuth.add(2147483647, -2147483648))  // 最大值+最小值
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))  // 正溢出
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 2))  // 负溢出
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))  // 最小值×-1
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))  // 空格
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 6
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(28, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：简单利息计算
  let principal = 10000  // 本金
  let rate = 5  // 利率（百分比）
  let time = 3  // 时间（年）
  
  // 利息 = 本金 × 利率 × 时间
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  // 总金额 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)  // 10000 + (10000×5×3) = 160000
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度（简化版）
  // F = C × 2 + 30 （避免浮点数运算）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}

test "sequence_operations" {
  // 测试序列运算：计算数列的和
  // 计算 1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  let total_sum = @azimuth.add(sum1, 5)
  @azimuth.assert_eq(15, total_sum)
  
  // 计算 2² + 3² + 4²
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let square4 = @azimuth.multiply(4, 4)
  let sum_of_squares = @azimuth.add(@azimuth.add(square2, square3), square4)
  @azimuth.assert_eq(29, sum_of_squares)
}

test "shopping_cart_calculation" {
  // 测试购物车计算场景
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(717, final_total)  // (199×2) + (299×1) + 20 = 717
}

test "assert_functions_various_types" {
  // 测试各种断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
  
  // 测试数学表达式
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
  @azimuth.assert_false(@azimuth.add(2, 3) == 6)
  @azimuth.assert_eq(@azimuth.multiply(4, 5), 20)
}