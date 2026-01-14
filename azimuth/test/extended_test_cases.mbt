test "add_function_edge_cases" {
  // 测试add函数的边界情况
  // 测试最大值边界
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 测试最小值边界
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(-1, @azimuth.add(min_val, max_val))
}

test "multiply_function_edge_cases" {
  // 测试multiply函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0相乘
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  
  // 测试与1相乘
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(1, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  
  // 测试与-1相乘
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 最小值特殊处理
  @azimuth.assert_eq(-max_val, @azimuth.multiply(-1, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))  // 最小值特殊处理
  
  // 测试极值相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-2, min_val))
}

test "greet_function_special_characters" {
  // 测试greet函数的特殊字符处理
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含制表符的字符串
  @azimuth.assert_eq_string("Hello, John	Doe!", @azimuth.greet("John	Doe"))
  
  // 测试包含各种标点符号的字符串
  @azimuth.assert_eq_string("Hello, John, Doe Jr.!", @azimuth.greet("John, Doe Jr."))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, John123!", @azimuth.greet("John123"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, João!", @azimuth.greet("João"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 北京!", @azimuth.greet("北京"))
  
  // 测试包含表情符号的字符串
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, 😀!", @azimuth.greet("😀"))
}

test "mathematical_combinations" {
  // 测试数学运算的组合
  
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 8
  let c = 9
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
  
  // 测试结合律：(a + b) + c = a + (b + c)
  let d = 100
  let e = 200
  let f = 300
  let left2 = @azimuth.add(@azimuth.add(d, e), f)
  let right2 = @azimuth.add(d, @azimuth.add(e, f))
  @azimuth.assert_eq(left2, right2)
  
  // 测试交换律：a + b = b + a 和 a * b = b * a
  let g = 42
  let h = -17
  @azimuth.assert_eq(@azimuth.add(g, h), @azimuth.add(h, g))
  @azimuth.assert_eq(@azimuth.multiply(g, h), @azimuth.multiply(h, g))
  
  // 测试平方计算：(a + b)² = a² + 2ab + b²
  let i = 5
  let j = 3
  let square_sum = @azimuth.multiply(@azimuth.add(i, j), @azimuth.add(i, j))
  let expanded = @azimuth.add(
    @azimuth.add(@azimuth.multiply(i, i), @azimuth.multiply(2, @azimuth.multiply(i, j))),
    @azimuth.multiply(j, j)
  )
  @azimuth.assert_eq(square_sum, expanded)
}

test "real_world_scenarios" {
  // 测试真实世界场景
  
  // 场景1：计算购物车总价
  let item1_price = 1599  // 商品1价格
  let item1_qty = 2       // 商品1数量
  let item2_price = 899   // 商品2价格
  let item2_qty = 1       // 商品2数量
  let item3_price = 299   // 商品3价格
  let item3_qty = 3       // 商品3数量
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let subtotal3 = @azimuth.multiply(item3_price, item3_qty)
  let total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), subtotal3)
  
  // 验证总价：1599*2 + 899*1 + 299*3 = 3198 + 899 + 897 = 4994
  @azimuth.assert_eq(4994, total)
  
  // 场景2：计算平均分
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let score5 = 89
  
  let sum_scores = @azimuth.add(@azimuth.add(
    @azimuth.add(score1, score2), 
    @azimuth.add(score3, score4)
  ), score5)
  
  // 计算平均值（使用整数除法）
  let average = sum_scores / 5
  @azimuth.assert_eq(88, average)  // (85+92+78+96+89)/5 = 440/5 = 88
  
  // 场景3：计算银行利息（简化版）
  let principal = 10000  // 本金
  let rate = 5           // 利率百分比
  let years = 3          // 年数
  
  // 简单利息计算：最终金额 = 本金 * (1 + 利率 * 年数 / 100)
  let interest_factor = @azimuth.add(100, @azimuth.multiply(rate, years))
  let final_amount = @azimuth.multiply(principal, interest_factor) / 100
  @azimuth.assert_eq(11500, final_amount)  // 10000 * (1 + 5*3/100) = 10000 * 1.15 = 11500
}

test "performance_oriented_tests" {
  // 测试性能相关的测试用例
  
  // 测试大量连续加法运算
  let mut sum = 0
  let mut i = 1
  while i <= 100 {
    sum = @azimuth.add(sum, i)
    i = @azimuth.add(i, 1)
  }
  // 1 + 2 + ... + 100 = 5050
  @azimuth.assert_eq(5050, sum)
  
  // 测试大量连续乘法运算（阶乘）
  let mut factorial = 1
  let mut j = 1
  while j <= 10 {
    factorial = @azimuth.multiply(factorial, j)
    j = @azimuth.add(j, 1)
  }
  // 10! = 3628800
  @azimuth.assert_eq(3628800, factorial)
  
  // 测试嵌套运算
  let result = @azimuth.multiply(
    @azimuth.add(10, 20),
    @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(5, 6))
  )
  // (10+20) * (3*4 + 5*6) = 30 * (12 + 30) = 30 * 42 = 1260
  @azimuth.assert_eq(1260, result)
}

test "error_handling_tests" {
  // 测试错误处理场景
  
  // 测试断言函数的正确使用
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("hello", "hello")
  
  // 测试比较操作
  @azimuth.assert_true(10 > 5)
  @azimuth.assert_true(5 < 10)
  @azimuth.assert_true(10 >= 10)
  @azimuth.assert_true(10 <= 10)
  @azimuth.assert_true(10 == 10)
  @azimuth.assert_true(10 != 5)
  
  @azimuth.assert_false(10 > 20)
  @azimuth.assert_false(5 < 0)
  @azimuth.assert_false(10 >= 20)
  @azimuth.assert_false(10 <= 5)
  @azimuth.assert_false(10 == 5)
  @azimuth.assert_false(10 != 10)
  
  // 测试字符串比较
  @azimuth.assert_true("hello" == "hello")
  @azimuth.assert_true("hello" != "world")
  @azimuth.assert_true("hello" > "hell")
  @azimuth.assert_true("hello" < "hellp")
}

test "boundary_value_tests" {
  // 测试边界值
  
  // 测试接近最大值的加法
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 1000))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000000, 1000000))
  
  // 测试接近最小值的加法
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000, -1000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000000, -1000000))
  
  // 测试接近最大值的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val / 2, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val / 3, 3))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val / 10, 10))
  
  // 测试接近最小值的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val / 2, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val / 3, 3))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val / 10, 10))
}

test "data_type_integrity_tests" {
  // 测试数据类型完整性
  
  // 测试零值的各种运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  
  // 测试单位元的各种运算
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  
  // 测试负数的各种运算
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
  
  // 测试字符串的空值处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "algorithm_complexity_tests" {
  // 测试算法复杂度相关的测试
  
  // 测试斐波那契数列计算（简化版）
  let fib_n = 10
  let mut a = 0
  let mut b = 1
  let mut i = 2
  while i <= fib_n {
    let temp = @azimuth.add(a, b)
    a = b
    b = temp
    i = @azimuth.add(i, 1)
  }
  // 第10个斐波那契数是55
  @azimuth.assert_eq(55, b)
  
  // 测试幂运算（使用乘法实现）
  let base = 3
  let exponent = 5
  let mut result = 1
  let mut j = 1
  while j <= exponent {
    result = @azimuth.multiply(result, base)
    j = @azimuth.add(j, 1)
  }
  // 3^5 = 243
  @azimuth.assert_eq(243, result)
  
  // 测试最大公约数算法（简化版，欧几里得算法）
  let m = 48
  let n = 18
  let mut a = m
  let mut b = n
  while b != 0 {
    let temp = b
    b = a % b  // 使用取模运算
    a = temp
  }
  // 48和18的最大公约数是6
  @azimuth.assert_eq(6, a)
}