// 额外的 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

test "arithmetic_sequence_sum" {
  // 计算等差数列的和：2 + 4 + 6 + 8 + 10
  let term1 = 2
  let term2 = 4
  let term3 = 6
  let term4 = 8
  let term5 = 10
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(30, sum)
}

test "compound_interest_calculation" {
  // 复利计算：本金 × (1 + 利率 × 年数)
  let principal = 1000
  let rate = 5
  let years = 3
  
  let growth_factor = add(1, multiply(rate, years))
  let total = multiply(principal, growth_factor)
  assert_eq(16000, total)
}

test "geometric_area_calculations" {
  // 几何面积计算
  let rectangle_length = 8
  let rectangle_width = 6
  let triangle_base = 10
  let triangle_height = 5
  
  // 矩形面积
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  assert_eq(48, rectangle_area)
  
  // 三角形面积
  let triangle_area = multiply(triangle_base, triangle_height) / 2
  assert_eq(25, triangle_area)
}

test "temperature_conversion" {
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "shopping_cart_total" {
  // 购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  let shipping = 15
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(add(total1, total2), shipping)
  
  assert_eq(704, cart_total)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  let empty_string = ""
  let single_char = "A"
  let unicode_string = "测试"
  let mixed_chars = "Test123!"
  
  assert_eq_string("Hello, !", greet(empty_string))
  assert_eq_string("Hello, A!", greet(single_char))
  assert_eq_string("Hello, 测试!", greet(unicode_string))
  assert_eq_string("Hello, Test123!", greet(mixed_chars))
}

test "mathematical_identities" {
  // 数学恒等式验证
  let x = 7
  let y = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "boundary_value_operations" {
  // 边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "nested_calculations" {
  // 嵌套计算：((a + b) × c) - (d × e)
  let a = 10
  let b = 5
  let c = 3
  let d = 4
  let e = 6
  
  let sum_ab = add(a, b)
  let product_abc = multiply(sum_ab, c)
  let product_de = multiply(d, e)
  let result = add(product_abc, -product_de)
  
  assert_eq(21, result)
}

test "conditional_logic_scenarios" {
  // 条件逻辑场景
  let score = 85
  let passing_score = 60
  let excellent_score = 90
  
  // 判断成绩等级
  let is_passing = score >= passing_score
  let is_excellent = score >= excellent_score
  
  assert_true(is_passing)
  assert_false(is_excellent)
  
  // 根据条件计算奖励分数
  let bonus = if is_excellent { 
    multiply(score, 2) 
  } else if is_passing { 
    add(score, 10) 
  } else { 
    score 
  }
  
  assert_eq(95, bonus)
}