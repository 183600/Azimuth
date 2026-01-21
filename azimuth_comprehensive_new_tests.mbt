// Azimuth 综合新测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_function_comprehensive_test" {
  // 测试加法函数的综合情况
  // 基本正数加法
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  
  // 负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  // 混合符号加法
  @azimuth.assert_eq(2, @azimuth.add(10, -8))
  @azimuth.assert_eq(-2, @azimuth.add(-10, 8))
  
  // 零值加法
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  // 基本乘法
  @azimuth.assert_eq(36, @azimuth.multiply(6, 6))
  
  // 负数乘法
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
  @azimuth.assert_eq(24, @azimuth.multiply(-4, -6))
  
  // 零乘法
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  
  // 单位元乘法
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(-123, @azimuth.multiply(-123, 1))
}

test "divide_with_ceil_comprehensive_test" {
  // 测试向上取整除法的综合情况
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
  
  // 负数被除数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))
  
  // 负数除数
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  
  // 双负数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
  
  // 零被除数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 零除数（应该返回0）
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "greet_function_string_handling" {
  // 测试问候函数的字符串处理
  // 基本问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  
  // 空格处理
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  
  // Unicode字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算组合
  // 嵌套运算：(a * b) + (c * d)
  let a = 5
  let b = 6
  let c = 7
  let d = 8
  
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(c, d)
  let result = @azimuth.add(product1, product2)
  @azimuth.assert_eq(86, result)  // 5*6 + 7*8 = 30 + 56 = 86
  
  // 链式运算：a + b + c + d
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(sum1, c)
  let final_sum = @azimuth.add(sum2, d)
  @azimuth.assert_eq(26, final_sum)  // 5 + 6 + 7 + 8 = 26
  
  // 混合运算：(a + b) * c
  let sum_ab = @azimuth.add(a, b)
  let mixed_result = @azimuth.multiply(sum_ab, c)
  @azimuth.assert_eq(77, mixed_result)  // (5 + 6) * 7 = 11 * 7 = 77
}

test "mathematical_properties_verification" {
  // 验证基本数学性质
  // 加法交换律：a + b = b + a
  let x = 123
  let y = 456
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 乘法分配律：a * (b + c) = a * b + a * c
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distrib_left, distrib_right)
}

test "boundary_value_testing" {
  // 边界值测试
  // 接近Int最大值的运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 安全的大数运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val + 1, -1))
  
  // 中等数值的边界测试
  let large_pos = 1000000
  let large_neg = -1000000
  @azimuth.assert_eq(0, @azimuth.add(large_pos, large_neg))
  @azimuth.assert_eq(-2000000, @azimuth.add(large_neg, large_neg))
  
  // 乘法边界测试
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(large_pos, large_pos))
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(large_neg, large_neg))
  @azimuth.assert_eq(-1000000000000, @azimuth.multiply(large_pos, large_neg))
}

test "assert_functions_comprehensive" {
  // 断言函数综合测试
  // 布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-1 == -1)
  @azimuth.assert_true(100 > 50)
  @azimuth.assert_true(-100 < -50)
  
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 == 1)
  @azimuth.assert_false(-1 == 1)
  @azimuth.assert_false(100 < 50)
  @azimuth.assert_false(-100 > -50)
  
  // 相等断言
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  
  // 字符串相等断言
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  @azimuth.assert_eq_string("12345", "12345")
  @azimuth.assert_eq_string("@#$%", "@#$%")
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 场景1：计算商品总价（含折扣和税费）
  let item_price = 100
  let quantity = 5
  let discount = 20  // 20元折扣
  let tax_rate = 3   // 3倍税率
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let discounted_price = @azimuth.add(subtotal, -discount)
  let final_price = @azimuth.multiply(discounted_price, tax_rate)
  @azimuth.assert_eq(1440, final_price)  // ((100 * 5) - 20) * 3 = 480 * 3 = 1440
  
  // 场景2：计算平均分（向上取整）
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 89
  let score5 = 94
  
  let total_score = @azimuth.add(score1, score2)
  total_score = @azimuth.add(total_score, score3)
  total_score = @azimuth.add(total_score, score4)
  total_score = @azimuth.add(total_score, score5)
  
  let average_score = @azimuth.divide_with_ceil(total_score, 5)
  @azimuth.assert_eq(88, average_score)  // ceil(438/5) = ceil(87.6) = 88
  
  // 场景3：生成个性化欢迎消息
  let user_name = "张三"
  let welcome_message = @azimuth.greet(user_name)
  @azimuth.assert_eq_string("Hello, 张三!", welcome_message)
  
  // 场景4：计算页数（向上取整）
  let total_items = 1000
  let items_per_page = 75
  let total_pages = @azimuth.divide_with_ceil(total_items, items_per_page)
  @azimuth.assert_eq(14, total_pages)  // ceil(1000/75) = ceil(13.33) = 14
}

test "error_handling_and_special_cases" {
  // 错误处理和特殊情况测试
  // 除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 极值乘以1或-1
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, abcdefghijklmnopqrstuvwxyz!", @azimuth.greet("abcdefghijklmnopqrstuvwxyz"))
}