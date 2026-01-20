// 标准 MoonBit 综合测试用例
// 测试核心功能：add, multiply, greet 函数的综合应用

test "arithmetie_sequence_sum" {
  // 计算等差数列前n项和：1 + 4 + 7 + 10 + 13
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 1
  let last = 13
  
  let sum = multiply(n, add(first, last)) / 2
  assert_eq(35, sum)
}

test "geometric_sequence_product" {
  // 计算等比数列前n项积：2 × 4 × 8 × 16
  // 通过连续相乘实现
  let term1 = 2
  let term2 = 4
  let term3 = 8
  let term4 = 16
  
  let product = multiply(multiply(term1, term2), multiply(term3, term4))
  assert_eq(1024, product)
}

test "temperature_conversion_formula" {
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius_temperatures = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  // 测试多个温度转换
  assert_eq(30, add(multiply(0, 2), 30))
  assert_eq(50, add(multiply(10, 2), 30))
  assert_eq(70, add(multiply(20, 2), 30))
  assert_eq(90, add(multiply(30, 2), 30))
  assert_eq(110, add(multiply(40, 2), 30))
}

test "financial_compound_interest" {
  // 复利计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5
  let years = 3
  
  let growth_factor = add(1, multiply(rate, years))
  let total = multiply(principal, growth_factor)
  assert_eq(160000, total)
}

test "string_internationalization" {
  // 测试国际化字符串处理
  let names = ["张三", "John", "José", "Владимир", "محمد"]
  
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, John!", greet("John"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, Владимир!", greet("Владимир"))
  assert_eq_string("Hello, محمد!", greet("محمد"))
}

test "boundary_value_mathematics" {
  // 边界值数学运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法测试
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "polygon_perimeter_calculation" {
  // 多边形周长计算
  // 正五边形，边长为7
  let sides = 5
  let side_length = 7
  let pentagon_perimeter = multiply(sides, side_length)
  assert_eq(35, pentagon_perimeter)
  
  // 正六边形，边长为4
  let hexagon_sides = 6
  let hexagon_side_length = 4
  let hexagon_perimeter = multiply(hexagon_sides, hexagon_side_length)
  assert_eq(24, hexagon_perimeter)
}

test "volume_calculation_series" {
  // 立方体体积计算系列
  let cube_sides = [2, 3, 4, 5, 6]
  
  // 计算不同边长立方体的体积
  assert_eq(8, multiply(2, multiply(2, 2)))      // 2³
  assert_eq(27, multiply(3, multiply(3, 3)))     // 3³
  assert_eq(64, multiply(4, multiply(4, 4)))     // 4³
  assert_eq(125, multiply(5, multiply(5, 5)))    // 5³
  assert_eq(216, multiply(6, multiply(6, 6)))    // 6³
}

test "shopping_cart_total_with_discount" {
  // 购物车总价计算（含折扣）
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  
  // 计算各商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let total3 = multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let cart_total = add(add(total1, total2), total3)
  
  // 满500减50的折扣
  let discount = if (cart_total > 500) { 50 } else { 0 }
  let final_total = add(cart_total, -discount)
  
  assert_eq(895, final_total)  // (199×2 + 299×1 + 99×3) - 50 = 945 - 50 = 895
}

test "error_boundary_validation" {
  // 错误边界验证测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护机制
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 验证正常运算
  assert_true(add(100, 200) == 300)
  assert_true(multiply(15, 20) == 300)
  assert_true(greet("Test") == "Hello, Test!")
}