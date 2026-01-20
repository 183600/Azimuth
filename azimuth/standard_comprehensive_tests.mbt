test "mathematical_properties_comprehensive" {
  // 测试数学性质和不变量
  let a = 42
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 加法结合律
  let c = 23
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "boundary_conditions_detailed" {
  // 详细测试边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 2, 1))
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 2, -1))
  
  // 测试最大值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "string_processing_comprehensive" {
  // 全面测试字符串处理
  // 基本字符串
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  
  // Unicode字符
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  
  // 数字和符号
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 正数除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(10, 1))
  
  // 负数除法
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  
  // 除数为负数
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(3, -2))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  
  // 零除法
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
}

test "complex_scenarios_practical" {
  // 实际应用场景测试
  // 场景1：计算购物车总价
  let item1 = 299
  let item2 = 199
  let item3 = 99
  let discount = -50
  let shipping = 15
  
  let subtotal = @azimuth.add(item1, item2)
  let subtotal2 = @azimuth.add(subtotal, item3)
  let discounted = @azimuth.add(subtotal2, discount)
  let total = @azimuth.add(discounted, shipping)
  
  @azimuth.assert_eq(552, total)
  
  // 场景2：批量订单计算
  let unit_price = 25
  let quantity = 12
  let bulk_discount = 0.8  // 80%的原价
  
  let order_total = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(300, order_total)
  
  // 场景3：分期付款计算
  let full_price = 1200
  let months = 12
  
  let monthly_payment = @azimuth.divide_with_ceil(full_price, months)
  @azimuth.assert_eq(100, monthly_payment)
  
  // 场景4：个性化问候
  let customer_names = ["Alice", "Bob", "Charlie"]
  let greeting1 = @azimuth.greet(customer_names[0])
  let greeting2 = @azimuth.greet(customer_names[1])
  let greeting3 = @azimuth.greet(customer_names[2])
  
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  @azimuth.assert_eq_string("Hello, Charlie!", greeting3)
}

test "performance_stress_operations" {
  // 性能压力测试
  // 连续加法操作
  let mut sum = 0
  sum = @azimuth.add(sum, 1000)
  sum = @azimuth.add(sum, 2000)
  sum = @azimuth.add(sum, 3000)
  sum = @azimuth.add(sum, 4000)
  sum = @azimuth.add(sum, 5000)
  @azimuth.assert_eq(15000, sum)
  
  // 连续乘法操作
  let mut product = 1
  product = @azimuth.multiply(product, 2)
  product = @azimuth.multiply(product, 3)
  product = @azimuth.multiply(product, 4)
  product = @azimuth.multiply(product, 5)
  @azimuth.assert_eq(120, product)
  
  // 混合运算压力测试
  let result1 = @azimuth.add(100, 200)
  let result2 = @azimuth.multiply(result1, 3)
  let result3 = @azimuth.add(result2, 150)
  let result4 = @azimuth.multiply(result3, 2)
  let result5 = @azimuth.add(result4, -100)
  
  @azimuth.assert_eq(1850, result5)
}

test "error_handling_robustness" {
  // 错误处理和健壮性测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极端值组合不会崩溃
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, min_val))
  
  // 测试大数与小数的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1000000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1000000))
  @azimuth.assert_eq(max_val, @azimuth.multiply(100000, 100000))
  
  // 测试断言函数的健壮性
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq(max_val, max_val)
  @azimuth.assert_eq(min_val, min_val)
}

test "algorithmic_complexity_tests" {
  // 算法复杂度测试
  // 测试数列求和
  let mut series_sum = 0
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  series_sum = @azimuth.add(series_sum, numbers[0])
  series_sum = @azimuth.add(series_sum, numbers[1])
  series_sum = @azimuth.add(series_sum, numbers[2])
  series_sum = @azimuth.add(series_sum, numbers[3])
  series_sum = @azimuth.add(series_sum, numbers[4])
  series_sum = @azimuth.add(series_sum, numbers[5])
  series_sum = @azimuth.add(series_sum, numbers[6])
  series_sum = @azimuth.add(series_sum, numbers[7])
  series_sum = @azimuth.add(series_sum, numbers[8])
  series_sum = @azimuth.add(series_sum, numbers[9])
  
  @azimuth.assert_eq(55, series_sum)
  
  // 测试阶乘计算
  let mut factorial = 1
  factorial = @azimuth.multiply(factorial, 1)
  factorial = @azimuth.multiply(factorial, 2)
  factorial = @azimuth.multiply(factorial, 3)
  factorial = @azimuth.multiply(factorial, 4)
  factorial = @azimuth.multiply(factorial, 5)
  
  @azimuth.assert_eq(120, factorial)
  
  // 测试幂运算（通过重复乘法）
  let base = 2
  let mut power = 1
  power = @azimuth.multiply(power, base)  // 2^1
  power = @azimuth.multiply(power, base)  // 2^2
  power = @azimuth.multiply(power, base)  // 2^3
  power = @azimuth.multiply(power, base)  // 2^4
  power = @azimuth.multiply(power, base)  // 2^5
  
  @azimuth.assert_eq(32, power)
}