// Azimuth 标准测试用例
// 使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "add_edge_cases" {
  // 测试加法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 测试最大值与最小值的加法
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "multiply_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试与-1的乘法
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "assert_functions" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "complex_operations" {
  // 测试复合运算
  let a = 10
  let b = 20
  let c = -5
  
  // 计算 (a * b) + c
  let product = @azimuth.multiply(a, b)
  let result = @azimuth.add(product, c)
  @azimuth.assert_eq(195, result)
  
  // 计算 a * (b + c)
  let sum = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum)
  @azimuth.assert_eq(150, result2)
}

test "add_negative_numbers" {
  // 测试负数加法的各种情况
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1073741824, -1073741824))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
}

test "multiply_by_zero_and_one" {
  // 测试乘法中的0和1的特殊处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -12345))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(1, -12345))
}

test "multiply_negative_one" {
  // 测试乘法中的-1特殊处理
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-1, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(-12345, -1))
  @azimuth.assert_eq(12345, @azimuth.multiply(-1, -12345))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-1, -2147483648))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "boolean_assertions" {
  // 测试布尔断言函数
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-1 == -1)
  @azimuth.assert_true(2147483647 == 2147483647)
  @azimuth.assert_true(-2147483648 == -2147483648)
  
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 == 1)
  @azimuth.assert_false(-1 == 1)
  @azimuth.assert_false(2147483647 == -2147483648)
  @azimuth.assert_false(-2147483648 == 2147483647)
}

// 新增的高质量测试用例
test "add_large_number_commutativity" {
  // 测试大数加法的交换律
  let large_pos = 1000000
  let large_neg = -1000000
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数交换律
  @azimuth.assert_eq(@azimuth.add(large_pos, max_val - 1000000), @azimuth.add(max_val - 1000000, large_pos))
  
  // 负数交换律
  @azimuth.assert_eq(@azimuth.add(large_neg, min_val + 1000000), @azimuth.add(min_val + 1000000, large_neg))
  
  // 混合符号交换律
  @azimuth.assert_eq(@azimuth.add(large_pos, large_neg), @azimuth.add(large_neg, large_pos))
}

test "add_associativity_with_overflow" {
  // 测试加法结合律在溢出情况下的行为
  let max_val = 2147483647
  let min_val = -2147483648
  let mid_val = 1000000
  
  // (max + mid) + min vs max + (mid + min)
  let result1 = @azimuth.add(@azimuth.add(max_val, mid_val), min_val)
  let result2 = @azimuth.add(max_val, @azimuth.add(mid_val, min_val))
  @azimuth.assert_eq(result1, result2)
}

test "multiply_power_of_two_edge_cases" {
  // 测试2的幂次方的乘法边界情况
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
  @azimuth.assert_eq(1048576, @azimuth.multiply(1024, 1024))
  @azimuth.assert_eq(-1024, @azimuth.multiply(-32, 32))
  @azimuth.assert_eq(1024, @azimuth.multiply(-32, -32))
  
  // 测试接近溢出的2的幂次方
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.multiply(65536, 32768))
}

test "multiply_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 123
  let b = 456
  let c = 789
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "divide_with_ceil_negative_dividend" {
  // 测试负数被除数的向上取整除法
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-6, 2))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-7, 2))
  @azimuth.assert_eq(-333334, @azimuth.divide_with_ceil(-1000000, 3))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负数除数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(11, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, -3))
}

test "greet_unicode_and_whitespace" {
  // 测试包含Unicode字符和空格的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列计算
  // 计算: 1 + 2*3 + 4*5 + 6*7 + 8*9
  let step1 = @azimuth.multiply(2, 3)
  let step2 = @azimuth.multiply(4, 5)
  let step3 = @azimuth.multiply(6, 7)
  let step4 = @azimuth.multiply(8, 9)
  
  let sum1 = @azimuth.add(1, step1)
  let sum2 = @azimuth.add(sum1, step2)
  let sum3 = @azimuth.add(sum2, step3)
  let result = @azimuth.add(sum3, step4)
  
  @azimuth.assert_eq(1 + 6 + 20 + 42 + 72, result)
}

test "extreme_value_combinations" {
  // 测试极值组合的运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法组合
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
  @azimuth.assert_eq(-2, @azimuth.add(min_val, min_val + 2))
  
  // 极值乘法组合
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 新增的测试用例
test "enhanced_add_function_basic_cases" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(1, 99))
}

test "enhanced_multiply_function_basic_cases" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(8, 0))
}

test "enhanced_greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "enhanced_divide_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "enhanced_divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 3))
}

test "enhanced_boolean_assertions_various_cases" {
  // 测试布尔断言的各种情况
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(-2 < 0)
  @azimuth.assert_true(10 == 10)
  @azimuth.assert_false(3 > 5)
  @azimuth.assert_false(-2 > 0)
  @azimuth.assert_false(10 == 5)
}

test "enhanced_complex_arithmetic_operations" {
  // 测试复杂的算术运算组合
  let a = 12
  let b = 8
  let c = 3
  
  // 计算 (a * b) + c
  let product = @azimuth.multiply(a, b)
  let result = @azimuth.add(product, c)
  @azimuth.assert_eq(99, result)
  
  // 计算 a * (b + c)
  let sum = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum)
  @azimuth.assert_eq(132, result2)
}

test "add_function_enhanced_boundary" {
  // 测试加法函数的增强边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -100))
  
  // 测试中等数值的加法
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(0, @azimuth.add(-500, 500))
}

test "multiply_function_enhanced_precision" {
  // 测试乘法函数的增强精度
  let sqrt_max = 46340
  
  // 测试接近溢出边界的乘法
  @azimuth.assert_eq(2147395600, @azimuth.multiply(sqrt_max, sqrt_max))
  @azimuth.assert_eq(-2147395600, @azimuth.multiply(sqrt_max, -sqrt_max))
  
  // 测试特殊值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -2147483648))
}

test "divide_with_ceil_enhanced_fractional" {
  // 测试向上取整除法的增强小数处理
  // 测试正数分数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 3))
  
  // 测试负数分数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))
}

test "greet_function_enhanced_unicode" {
  // 测试问候函数的增强Unicode处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
}

test "complex_mathematical_operations" {
  // 测试复杂的数学运算组合
  // 嵌套运算测试
  @azimuth.assert_eq(25, @azimuth.add(@azimuth.multiply(3, 5), @azimuth.multiply(2, 5)))
  @azimuth.assert_eq(20, @azimuth.multiply(@azimuth.add(3, 2), 4))
  @azimuth.assert_eq(8, @azimuth.divide_with_ceil(@azimuth.add(@azimuth.multiply(2, 3), 2), 2))
  
  // 多层嵌套测试
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.multiply(@azimuth.divide_with_ceil(10, 2), 2), 5))
}

test "assert_functions_comprehensive" {
  // 断言函数综合测试
  @azimuth.assert_true(1 + 1 == 2)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-1 == -1)
  
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 == 1)
  @azimuth.assert_false(-1 == 1)
  
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq(0, 0)
  
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  // 场景1：计算购物车总价（含税）
  let item1 = 299
  let item2 = 399
  let item3 = 199
  let tax_rate = 3
  
  let subtotal = @azimuth.add(@azimuth.add(item1, item2), item3)
  let total_with_tax = @azimuth.multiply(subtotal, tax_rate)
  @azimuth.assert_eq(2691, total_with_tax)
  
  // 场景2：计算平均分（向上取整）
  let score1 = 87
  let score2 = 93
  let score3 = 79
  let score4 = 85
  
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4)
  let average_score = @azimuth.divide_with_ceil(total_score, 4)
  @azimuth.assert_eq(87, average_score)
  
  // 场景3：用户欢迎消息
  let username = "张三"
  let welcome_msg = @azimuth.greet(username)
  @azimuth.assert_eq_string("Hello, 张三!", welcome_msg)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(123, 456), @azimuth.add(456, 123))
  @azimuth.assert_eq(@azimuth.add(-100, 200), @azimuth.add(200, -100))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(12, 34), @azimuth.multiply(34, 12))
  @azimuth.assert_eq(@azimuth.multiply(-5, 7), @azimuth.multiply(7, -5))
  
  // 结合律验证（在安全范围内）
  let a = 100
  let b = 200
  let c = 300
  
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}