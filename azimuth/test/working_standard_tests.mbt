test "add_function_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "multiply_function_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}

test "greet_function_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "divide_with_ceil_test" {
  let result = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result)
}

test "subtract_function_test" {
  let result = @azimuth.subtract(10, 3)
  @azimuth.assert_eq(7, result)
}

test "math_commutative_test" {
  let result1 = @azimuth.add(5, 7)
  let result2 = @azimuth.add(7, 5)
  @azimuth.assert_eq(result1, result2)
}

test "zero_element_test" {
  let add_result = @azimuth.add(5, 0)
  let multiply_result = @azimuth.multiply(5, 0)
  @azimuth.assert_eq(5, add_result)
  @azimuth.assert_eq(0, multiply_result)
}

test "complex_arithmetic_test" {
  let step1 = @azimuth.multiply(3, 4)
  let step2 = @azimuth.add(step1, 5)
  let result = @azimuth.divide_with_ceil(step2, 2)
  @azimuth.assert_eq(9, result)  // ceil((3*4+5)/2) = ceil(17/2) = 9
}

test "packaging_scenario_test" {
  let items = 25
  let capacity = 6
  let packages = @azimuth.divide_with_ceil(items, capacity)
  let total_capacity = @azimuth.multiply(packages, capacity)
  let remaining = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(5, packages)
  @azimuth.assert_eq(5, remaining)
}

test "unicode_greeting_test" {
  let result1 = @azimuth.greet("世界")
  let result2 = @azimuth.greet("🚀")
  @azimuth.assert_eq_string("Hello, 世界!", result1)
  @azimuth.assert_eq_string("Hello, 🚀!", result2)
}

test "enhanced_add_large_numbers" {
  // 测试大数相加
  let max_val = 2147483647
  
  // 接近最大值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 1147483647))
  @azimuth.assert_eq(max_val, @azimuth.add(2000000000, 147483647))
  
  // 对称性测试
  @azimuth.assert_eq(@azimuth.add(123456789, 987654321), @azimuth.add(987654321, 123456789))
}

test "enhanced_multiply_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘但不溢出
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(99980001, @azimuth.multiply(9999, 9999))
  
  // 溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(50000, 50000))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-50000, 50000))
}

test "enhanced_complex_arithmetic" {
  // 测试复合算术表达式
  let x = 15
  let y = 25
  let z = -10
  
  // (x + y) * z
  let sum = @azimuth.add(x, y)
  let result1 = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(-400, result1)
  
  // x * y + z
  let product = @azimuth.multiply(x, y)
  let result2 = @azimuth.add(product, z)
  @azimuth.assert_eq(365, result2)
}

test "enhanced_mathematical_properties" {
  // 测试数学性质
  
  // 加法交换律
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  
  // 单位元素性质
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "enhanced_greet_complex_strings" {
  // 测试复杂字符串处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, spaced!", @azimuth.greet("spaced"))
  @azimuth.assert_eq_string("Hello, quoted!", @azimuth.greet("quoted"))
}

test "enhanced_real_world_scenarios" {
  // 测试实际应用场景
  
  // 购物车计算
  let item1_price = 199
  let item2_price = 299
  let item3_price = 399
  let discount = -50
  
  let subtotal = @azimuth.add(item1_price, item2_price)
  let subtotal2 = @azimuth.add(subtotal, item3_price)
  let total = @azimuth.add(subtotal2, discount)
  @azimuth.assert_eq(847, total)
  
  // 用户个性化问候
  let user_name = "张三"
  let admin_name = "Admin"
  let user_greeting = @azimuth.greet(user_name)
  let admin_greeting = @azimuth.greet(admin_name)
  @azimuth.assert_eq_string("Hello, 张三!", user_greeting)
  @azimuth.assert_eq_string("Hello, Admin!", admin_greeting)
}

test "enhanced_performance_operations" {
  // 测试性能相关操作
  
  // 重复运算测试
  let base = 1000
  let result1 = @azimuth.add(base, base)
  let result2 = @azimuth.add(result1, base)
  let result3 = @azimuth.add(result2, base)
  let result4 = @azimuth.add(result3, base)
  @azimuth.assert_eq(5000, result4)
  
  // 连续乘法测试
  let mult_base = 2
  let mult_result1 = @azimuth.multiply(mult_base, 2)
  let mult_result2 = @azimuth.multiply(mult_result1, 2)
  let mult_result3 = @azimuth.multiply(mult_result2, 2)
  let mult_result4 = @azimuth.multiply(mult_result3, 2)
  @azimuth.assert_eq(32, mult_result4)
  
  // 混合运算压力测试
  let mixed1 = @azimuth.add(100, 200)
  let mixed2 = @azimuth.multiply(mixed1, 3)
  let mixed3 = @azimuth.add(mixed2, 150)
  let mixed4 = @azimuth.multiply(mixed3, 2)
  @azimuth.assert_eq(2100, mixed4)
}

test "enhanced_error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1000000000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1000000000))
  
  // 乘法边界
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(65536, 65536))
}

test "enhanced_function_composition" {
  // 测试函数组合
  
  // 计算数列的和
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let total = @azimuth.add(sum3, 5)
  @azimuth.assert_eq(15, total)
  
  // 计算阶乘（简化版）
  let fact2 = @azimuth.multiply(2, 1)
  let fact3 = @azimuth.multiply(3, fact2)
  let fact4 = @azimuth.multiply(4, fact3)
  @azimuth.assert_eq(24, fact4)
  
  // 字符串处理链
  let greeting1 = @azimuth.greet("User")
  let greeting2 = @azimuth.greet("Admin")
  @azimuth.assert_eq_string("Hello, User!", greeting1)
  @azimuth.assert_eq_string("Hello, Admin!", greeting2)
}
test "user_added_test" {
  let result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result)
}
test "user_add_with_zero" {
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}
test "user_add_positive_and_negative" {
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(3, @azimuth.add(10, -7))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
}
test "user_multiply_with_one_and_negative_one" {
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
  @azimuth.assert_eq(-7, @azimuth.multiply(-1, 7))
}
test "user_greet_with_special_characters" {
  @azimuth.assert_eq_string("Hello, test@123!", @azimuth.greet("test@123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}
test "user_greet_with_empty_string" {
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}
test "user_combined_operations" {
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(5, 6)
  let greeting = @azimuth.greet("Test")
  
  @azimuth.assert_eq(30, sum)
  @azimuth.assert_eq(30, product)
  @azimuth.assert_eq_string("Hello, Test!", greeting)
}

// 新增的标准测试用例
test "add_zero_edge_cases" {
  // 测试加法的零值边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(0, -2147483648))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 1))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 2147483647))
  @azimuth.assert_eq(2147483647, @azimuth.add(1000000000, 2000000000))
}

test "add_underflow_protection" {
  // 测试加法下溢保护
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -2147483648))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1000000000, -2000000000))
}

test "multiply_zero_and_one" {
  // 测试乘法的零值和一值情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(1000000, 3000))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
}

test "multiply_min_value_special_cases" {
  // 测试最小值乘法的特殊情况
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(2, -2147483648))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !@#$!", @azimuth.greet("!@#$"))
  @azimuth.assert_eq_string("Hello, 中文字符!", @azimuth.greet("中文字符"))
  @azimuth.assert_eq_string("Hello, emoji😊!", @azimuth.greet("emoji😊"))
}

test "greet_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, \n!", @azimuth.greet("\n"))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  // (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(75, result)
  
  // 测试嵌套运算
  let x = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.add(2, 4))
  @azimuth.assert_eq(48, x)
}

// 新添加的标准测试用例
test "extreme_boundary_addition" {
  // 测试接近边界的值
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
  
  // 测试边界值与接近边界值的组合
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(2147483647, @azimuth.add(1, 2147483646))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1, -2147483647))
}

test "extreme_boundary_multiplication" {
  // 测试大数相乘
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46340))
  
  // 测试负数大数相乘
  @azimuth.assert_eq(2147483647, @azimuth.multiply(-46340, -46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46340, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(46340, -46341))
}

test "mixed_arithmetic_operations" {
  // 测试连续加法
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  // 测试连续乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 测试加法和乘法混合（模拟优先级）
  let a = 2
  let b = 3
  let c = 4
  let d = 5
  
  // (a + b) * (c + d)
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let result3 = @azimuth.multiply(sum1, sum2)
  @azimuth.assert_eq(35, result3)
  
  // a * b + c * d
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(c, d)
  let result4 = @azimuth.add(product1, product2)
  @azimuth.assert_eq(26, result4)
}

test "string_processing_edge_cases" {
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result)
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
  
  // 测试包含反斜杠的字符串
  @azimuth.assert_eq_string("Hello, \\path\\!", @azimuth.greet("\\path\\"))
  
  // 测试包含各种空白字符的字符串
  @azimuth.assert_eq_string("Hello, \t\n\r!", @azimuth.greet("\t\n\r"))
}

test "negative_number_operations" {
  // 测试负数加法的各种组合
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-7, @azimuth.add(-10, 3))
  @azimuth.assert_eq(3, @azimuth.add(-7, 10))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  
  // 测试负数乘法的各种组合
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "zero_value_behavior" {
  // 测试加法中的零值
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 测试乘法中的零值
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  
  // 测试零值与边界值的组合
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
}

test "cumulative_operations" {
  // 测试连续加法的累积效应
  let mut sum = 0
  sum = @azimuth.add(sum, 1)
  sum = @azimuth.add(sum, 2)
  sum = @azimuth.add(sum, 3)
  sum = @azimuth.add(sum, 4)
  sum = @azimuth.add(sum, 5)
  @azimuth.assert_eq(15, sum)
  
  // 测试连续乘法的累积效应
  let mut product = 1
  product = @azimuth.multiply(product, 2)
  product = @azimuth.multiply(product, 3)
  product = @azimuth.multiply(product, 4)
  @azimuth.assert_eq(24, product)
  
  // 测试混合连续运算
  let mut result = 10
  result = @azimuth.add(result, 5)  // 15
  result = @azimuth.multiply(result, 2)  // 30
  result = @azimuth.add(result, -10)  // 20
  result = @azimuth.multiply(result, 3)  // 60
  @azimuth.assert_eq(60, result)
}

test "practical_application_scenarios" {
  // 场景1：计算购物车总价（使用加法）
  let item1_price = 199
  let item2_price = 299
  let item3_price = 99
  let shipping = 15
  let total = @azimuth.add(@azimuth.add(@azimuth.add(item1_price, item2_price), item3_price), shipping)
  @azimuth.assert_eq(612, total)
  
  // 场景2：计算订单总量（单价 × 数量）
  let unit_price = 25
  let quantity = 8
  let order_total = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(200, order_total)
  
  // 场景3：生成个性化问候消息
  let customer_name = "张三"
  let greeting = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
  
  // 场景4：计算折扣价格（原价 × 折扣率）
  let original_price = 1000
  let discount_rate = 8  // 表示80%折扣（保留20%）
  let discounted_price = @azimuth.multiply(original_price, discount_rate)
  // 注意：这里假设discount_rate已经是百分比形式，实际应用中可能需要除法
  @azimuth.assert_eq(8000, discounted_price)
}

// 新添加的测试用例
test "add_zero_handling_extended" {
  // 测试加法的零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
}

test "add_boundary_values_extended" {
  // 测试加法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
}

test "multiply_zero_and_one_extended" {
  // 测试乘法的0和1处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(123, @azimuth.multiply(1, 123))
  @azimuth.assert_eq(-123, @azimuth.multiply(123, -1))
  @azimuth.assert_eq(-123, @azimuth.multiply(-1, 123))
}

test "multiply_negative_numbers_extended" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-3, 0))
}

test "multiply_boundary_values_extended" {
  // 测试乘法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
}

test "greet_edge_cases_extended" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, abcdef!", @azimuth.greet("abcdef"))
}

test "greet_special_characters_extended" {
  // 测试问候函数的特殊字符
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "complex_add_operations_extended" {
  // 测试复杂的加法运算
  @azimuth.assert_eq(100, @azimuth.add(25, 75))
  @azimuth.assert_eq(-100, @azimuth.add(-25, -75))
  @azimuth.assert_eq(50, @azimuth.add(100, -50))
  @azimuth.assert_eq(-50, @azimuth.add(-100, 50))
}

test "complex_multiply_operations_extended" {
  // 测试复杂的乘法运算
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(1000, @azimuth.multiply(25, 40))
}

test "assertion_functionality_extended" {
  // 测试断言功能
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 < 1)
  @azimuth.assert_true(-1 < 0)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 > 1)
  @azimuth.assert_false(-1 > 0)
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq_string("", "")
}

// 用户添加的测试用例
test "user_added_edge_case_addition" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "user_added_edge_case_multiplication" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "user_added_divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-12, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-13, 3))
  
  // 除数为负数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(11, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(13, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
}

test "user_added_mixed_operations" {
  // 测试混合运算
  let a = 10
  let b = 5
  let c = -3
  
  // 加法和乘法混合
  @azimuth.assert_eq(50, @azimuth.multiply(a, @azimuth.add(b, c)))
  @azimuth.assert_eq(20, @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 连续加法
  @azimuth.assert_eq(12, @azimuth.add(@azimuth.add(a, b), c))
  
  // 连续乘法
  @azimuth.assert_eq(-150, @azimuth.multiply(@azimuth.multiply(a, b), c))
}

test "user_added_zero_and_one_operations" {
  // 测试0和1的特殊运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-1, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(-1, -12345))
  
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.add(12345, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

// 新增的增强测试用例
test "add_large_numbers_enhanced" {
  // 测试大数加法
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(0, @azimuth.add(1000000, -1000000))
}

test "multiply_fractional_logic_enhanced" {
  // 测试乘法的分数逻辑（虽然都是整数，但测试类似分数的概念）
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(36, @azimuth.multiply(6, 6))
  @azimuth.assert_eq(49, @azimuth.multiply(7, 7))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
}

test "greet_unicode_and_spaces_enhanced" {
  // 测试包含Unicode字符和空格的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit 语言!", @azimuth.greet("MoonBit 语言"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, test user!", @azimuth.greet("test user"))
}

test "complex_arithmetic_sequence_enhanced" {
  // 测试复杂的算术序列
  let result1 = @azimuth.add(1, 1)  // 2
  let result2 = @azimuth.multiply(result1, 2)  // 4
  let result3 = @azimuth.add(result2, 1)  // 5
  let result4 = @azimuth.multiply(result3, 3)  // 15
  let result5 = @azimuth.divide_with_ceil(result4, 4)  // 4
  @azimuth.assert_eq(4, result5)
}

// 新增的测试用例 - 极值边界条件测试
test "add_function_extreme_boundary_values" {
  // 测试极值边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 2, 1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 2, -1))
}

// Unicode字符和特殊字符测试
test "greet_function_unicode_and_special_chars" {
  // 测试Unicode字符和特殊字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, a@b.com!", @azimuth.greet("a@b.com"))
}

// 向上取整除法全面测试
test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 100))
  
  // 零和负除数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

// 新增的高质量测试用例 - 10个标准 MoonBit 测试用例
test "azimuth_arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-1, @azimuth.add(5, -6))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
}

test "azimuth_divide_with_ceil_edge_cases" {
  // 向上取整除法的边界情况测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-3, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(3, -2))
}

test "azimuth_greet_function_internationalization" {
  // 问候函数的国际化测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, world!", @azimuth.greet("world"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "azimuth_mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 3)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  )
  
  // 减法的逆运算
  @azimuth.assert_eq(x, @azimuth.add(@azimuth.subtract(x, y), y))
}

test "azimuth_business_logic_packaging_optimization" {
  // 业务逻辑：包装优化测试
  let total_items = 47
  let box_capacity = 12
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  @azimuth.assert_eq(4, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(48, total_capacity)
  
  let wasted_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(1, wasted_space)
  
  let space_utilization = @azimuth.multiply(@azimuth.divide_with_ceil(total_items, total_capacity), 100)
  @azimuth.assert_eq(98, space_utilization)
}

test "azimuth_complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let offset = 3
  
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, offset)
  
  @azimuth.assert_eq(12, result)  // ceil(15*4/7) + 3 = 9 + 3 = 12
}

test "azimuth_error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值处理
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000000, 1000000))
}

test "azimuth_resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let projects = 3
  let min_resources_per_project = @azimuth.divide_with_ceil(total_resources, projects)
  
  @azimuth.assert_eq(34, min_resources_per_project)
  
  let total_allocated = @azimuth.multiply(min_resources_per_project, projects)
  @azimuth.assert_eq(102, total_allocated)
  
  let resource_overhead = @azimuth.subtract(total_allocated, total_resources)
  @azimuth.assert_eq(2, resource_overhead)
}

test "azimuth_performance_benchmark_operations" {
  // 性能基准操作测试
  let large_number = 10000
  
  let sum_result = @azimuth.add(large_number, large_number)
  let product_result = @azimuth.multiply(large_number, 100)
  let division_result = @azimuth.divide_with_ceil(product_result, large_number)
  
  @azimuth.assert_eq(20000, sum_result)
  @azimuth.assert_eq(1000000, product_result)
  @azimuth.assert_eq(100, division_result)
}

test "azimuth_real_world_application_comprehensive" {
  // 真实世界应用综合测试
  // 场景：活动安排和资源管理
  let participants = 87
  let chairs_per_table = 10
  let budget_per_table = 50
  
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  let empty_chairs = @azimuth.subtract(total_chairs, participants)
  @azimuth.assert_eq(3, empty_chairs)
  
  let total_budget = @azimuth.multiply(tables_needed, budget_per_table)
  @azimuth.assert_eq(450, total_budget)
  
  let budget_per_person = @azimuth.divide_with_ceil(total_budget, participants)
  @azimuth.assert_eq(6, budget_per_person)
}

// 新增的 MoonBit 测试用例 - 不超过10个
test "moonbit_enhanced_arithmetic_test" {
  // 增强算术测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
}

test "moonbit_string_processing_test" {
  // 字符串处理测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "moonbit_business_calculation_test" {
  // 业务计算测试
  let items = 47
  let capacity = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, capacity)
  @azimuth.assert_eq(4, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, capacity)
  @azimuth.assert_eq(48, total_capacity)
  
  // 使用加法和负数代替减法
  let remaining = @azimuth.add(total_capacity, -items)
  @azimuth.assert_eq(1, remaining)
}

test "moonbit_error_handling_test" {
  // 错误处理测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
}

test "moonbit_complex_calculation_test" {
  // 复杂计算测试
  let base = 15
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  let step1 = @azimuth.multiply(base, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(17, result)  // ceil(15*3/4) + 5 = 12 + 5 = 17
}

test "moonbit_mathematical_properties_test" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "moonbit_financial_calculation_test" {
  // 金融计算测试
  let loan_amount = 12000
  let monthly_payment = 1000
  
  let months_needed = @azimuth.divide_with_ceil(loan_amount, monthly_payment)
  @azimuth.assert_eq(12, months_needed)
  
  let total_payment = @azimuth.multiply(months_needed, monthly_payment)
  @azimuth.assert_eq(12000, total_payment)
}

test "moonbit_resource_allocation_test" {
  // 资源分配测试
  let total_resources = 500
  let teams = 4
  let resources_per_team = @azimuth.divide_with_ceil(total_resources, teams)
  @azimuth.assert_eq(125, resources_per_team)
  
  let total_allocated = @azimuth.multiply(resources_per_team, teams)
  @azimuth.assert_eq(500, total_allocated)
}

test "moonbit_event_planning_test" {
  // 活动规划测试
  let attendees = 87
  let chairs_per_table = 10
  
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  // 使用加法和负数代替减法
  let empty_chairs = @azimuth.add(total_chairs, -attendees)
  @azimuth.assert_eq(3, empty_chairs)
}

test "moonbit_algorithm_performance_test" {
  // 算法性能测试
  let input_size = 1000
  
  // 线性算法模拟
  let linear_steps = @azimuth.multiply(input_size, 5)
  @azimuth.assert_eq(5000, linear_steps)
  
  // 对数算法模拟
  let log_steps = @azimuth.divide_with_ceil(input_size, 100)
  @azimuth.assert_eq(10, log_steps)
}

// 新增的增强测试用例
test "enhanced_arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(7, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "enhanced_mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 2)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2))
  )
  
  // 减法验证
  @azimuth.assert_eq(4, @azimuth.subtract(12, 8))
  @azimuth.assert_eq(-4, @azimuth.subtract(8, 12))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
}

test "enhanced_string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

test "enhanced_business_logistics_calculation" {
  // 物流业务计算测试
  let total_packages = 87
  let packages_per_box = 12
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_packages, packages_per_box)
  @azimuth.assert_eq(8, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, packages_per_box)
  @azimuth.assert_eq(96, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_packages)
  @azimuth.assert_eq(9, remaining_space)
}

test "enhanced_financial_calculation_scenario" {
  // 金融计算场景测试
  let loan_amount = 15000
  let monthly_payment = 1200
  
  // 计算需要的月份数
  let months_needed = @azimuth.divide_with_ceil(loan_amount, monthly_payment)
  @azimuth.assert_eq(13, months_needed)
  
  // 验证总支付金额
  let total_payment = @azimuth.multiply(months_needed, monthly_payment)
  @azimuth.assert_eq(15600, total_payment)
  
  // 验证最后一个月的实际支付金额
  let final_payment = @azimuth.subtract(total_payment, @azimuth.multiply(months_needed - 1, monthly_payment))
  @azimuth.assert_eq(600, final_payment)
}

test "enhanced_complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 20
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(22, result)  // ceil(20*3/4) + 7 = 15 + 7 = 22
}

test "enhanced_event_planning_optimization" {
  // 活动规划优化测试
  let attendees = 125
  let tables_available = 15
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(16, tables_needed)
  
  // 检查桌子是否足够
  @azimuth.assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(128, total_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, attendees)
  @azimuth.assert_eq(3, remaining_seats)
}

test "enhanced_error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的全面测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
  
  // 极值运算测试
  let max_val = 1000000
  let min_val = -1000000
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "enhanced_resource_allocation_efficiency" {
  // 资源分配效率测试
  let total_resources = 500
  let teams = 4
  let min_resources_per_team = @azimuth.divide_with_ceil(total_resources, teams)
  
  @azimuth.assert_eq(125, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = @azimuth.multiply(min_resources_per_team, teams)
  @azimuth.assert_eq(500, allocated_total)
  
  // 验证资源利用率
  let utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_resources, 100), allocated_total)
  @azimuth.assert_eq(100, utilization)
}

test "enhanced_algorithm_performance_validation" {
  // 算法性能验证测试
  let input_size = 1000
  
  // 线性算法模拟
  let linear_steps = @azimuth.multiply(input_size, 5)
  @azimuth.assert_eq(5000, linear_steps)
  
  // 对数算法模拟（使用向上取整除法模拟）
  let log_steps = @azimuth.divide_with_ceil(input_size, 100)
  @azimuth.assert_eq(10, log_steps)
  
  // 验证算法效率差异
  let efficiency_ratio = @azimuth.divide_with_ceil(linear_steps, log_steps)
  @azimuth.assert_eq(500, efficiency_ratio)
}
