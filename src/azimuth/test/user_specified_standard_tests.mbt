// 用户指定的标准 MoonBit 测试用例
// 包含10个标准测试用例，使用标准的 MoonBit 测试语法

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
}

test "greet_function_variations" {
  // 测试问候函数的各种变化
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "mathematical_associative_properties" {
  // 测试数学结合律
  let a = 5
  let b = 10
  let c = 15
  
  // 加法结合律：(a + b) + c = a + (b + c)
  let left_add = @azimuth.add(@azimuth.add(a, b), c)
  let right_add = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_add, right_add)
  
  // 乘法结合律：(a × b) × c = a × (b × c)
  let left_mul = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mul = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mul, right_mul)
}

test "boundary_value_operations" {
  // 测试边界值操作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算 (a + b) × (c - d)
  let a = 12
  let b = 8
  let c = 20
  let d = 5
  
  let sum = @azimuth.add(a, b)  // 20
  let diff = @azimuth.add(c, -d)  // 15
  let result = @azimuth.multiply(sum, diff)  // 300
  @azimuth.assert_eq(300, result)
  
  // 场景2：计算 a × b + c × d
  let e = 6
  let f = 7
  let g = 4
  let h = 9
  
  let product1 = @azimuth.multiply(e, f)  // 42
  let product2 = @azimuth.multiply(g, h)  // 36
  let total = @azimuth.add(product1, product2)  // 78
  @azimuth.assert_eq(78, total)
}

test "real_world_shopping_cart" {
  // 测试实际购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  let shipping_fee = 15
  let discount = 50
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 297
  
  // 计算商品总价
  let subtotal = @azimuth.add(total1, total2)  // 695
  
  // 应用折扣
  let discounted_total = @azimuth.add(subtotal, -discount)  // 645
  
  // 添加运费
  let final_total = @azimuth.add(discounted_total, shipping_fee)  // 660
  
  @azimuth.assert_eq(660, final_total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 长字符串
  let long_name = "VeryLongNameThatExceedsNormalLength"
  @azimuth.assert_eq_string("Hello, VeryLongNameThatExceedsNormalLength!", @azimuth.greet(long_name))
  
  // 包含特殊字符
  let special_name = "user@domain.com"
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet(special_name))
  
  // 包含空格
  let spaced_name = "John Smith"
  @azimuth.assert_eq_string("Hello, John Smith!", @azimuth.greet(spaced_name))
  
  // 包含数字和字母
  let mixed_name = "User123"
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(mixed_name))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 7
  let sum = 0
  let i = 1
  while (i <= n) {
    let term = @azimuth.add(@azimuth.multiply(2, i), -1)
    sum = @azimuth.add(sum, term)
    i = @azimuth.add(i, 1)
  }
  @azimuth.assert_eq(@azimuth.multiply(n, n), sum)  // 7² = 49
  
  // 计算等比数列前n项和：2 + 4 + 8 + ... + 2^n
  let m = 5
  let geo_sum = 0
  let term = 2
  let j = 1
  while (j <= m) {
    geo_sum = @azimuth.add(geo_sum, term)
    term = @azimuth.multiply(term, 2)
    j = @azimuth.add(j, 1)
  }
  @azimuth.assert_eq(62, geo_sum)  // 2 + 4 + 8 + 16 + 32 = 62
}

test "temperature_conversion_calculations" {
  // 测试温度转换计算（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let celsius_temps = [@azimuth.add(0, 0), @azimuth.add(10, 0), @azimuth.add(25, 0), @azimuth.add(100, 0)]
  let expected_fahrenheit = [@azimuth.add(30, 0), @azimuth.add(50, 0), @azimuth.add(80, 0), @azimuth.add(230, 0)]
  
  let index = 0
  while (index < 4) {
    let celsius = celsius_temps[index]
    let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
    @azimuth.assert_eq(expected_fahrenheit[index], fahrenheit)
    index = @azimuth.add(index, 1)
  }
  
  // 华氏度转摄氏度：C = (F - 30) ÷ 2（简化公式）
  let fahrenheit_temps = [@azimuth.add(32, 0), @azimuth.add(50, 0), @azimuth.add(68, 0), @azimuth.add(212, 0)]
  let expected_celsius = [@azimuth.add(1, 0), @azimuth.add(10, 0), @azimuth.add(19, 0), @azimuth.add(91, 0)]
  
  let index2 = 0
  while (index2 < 4) {
    let fahrenheit = fahrenheit_temps[index2]
    let celsius = @azimuth.add(fahrenheit, -30) / 2
    @azimuth.assert_eq(expected_celsius[index2], celsius)
    index2 = @azimuth.add(index2, 1)
  }
}