// Azimuth 高质量测试套件
// 使用标准的 MoonBit 测试语法，包含全面的测试覆盖

test "add_function_comprehensive_test" {
  // 加法函数综合测试
  // 基本正数加法
  assert_eq(15, add(7, 8))
  assert_eq(100, add(50, 50))
  
  // 负数加法
  assert_eq(-15, add(-7, -8))
  assert_eq(-100, add(-50, -50))
  
  // 混合符号加法
  assert_eq(-1, add(-7, 6))
  assert_eq(1, add(7, -6))
  
  // 零值加法
  assert_eq(7, add(7, 0))
  assert_eq(-7, add(-7, 0))
  assert_eq(0, add(0, 0))
}

test "multiply_function_comprehensive_test" {
  // 乘法函数综合测试
  // 基本正数乘法
  assert_eq(42, multiply(6, 7))
  assert_eq(100, multiply(10, 10))
  
  // 负数乘法
  assert_eq(42, multiply(-6, -7))
  assert_eq(-42, multiply(-6, 7))
  assert_eq(-42, multiply(6, -7))
  
  // 零值乘法
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(0, -12345))
  
  // 单位乘法
  assert_eq(12345, multiply(12345, 1))
  assert_eq(-12345, multiply(-12345, 1))
}

test "divide_with_ceil_edge_cases_test" {
  // 向上取整除法边界情况测试
  // 基本除法
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  
  // 负数被除数
  assert_eq(-3, divide_with_ceil(-9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  
  // 负数除数
  assert_eq(-3, divide_with_ceil(9, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(-4, divide_with_ceil(11, -3))
  
  // 零被除数
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  
  // 零除数（根据实现应返回0）
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "greet_function_string_handling_test" {
  // 问候函数字符串处理测试
  // 基本字符串
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, Alice!", greet("Alice"))
  
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  
  // 长字符串
  assert_eq_string("Hello, VeryLongUserName!", greet("VeryLongUserName"))
  
  // 包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,   !", greet("   "))
}

test "mathematical_properties_verification_test" {
  // 数学性质验证测试
  // 加法交换律 (在安全范围内)
  assert_eq(add(123, 456), add(456, 123))
  assert_eq(add(-100, 200), add(200, -100))
  assert_eq(add(0, 500), add(500, 0))
  
  // 乘法交换律 (在安全范围内)
  assert_eq(multiply(12, 34), multiply(34, 12))
  assert_eq(multiply(-5, 7), multiply(7, -5))
  assert_eq(multiply(0, 100), multiply(100, 0))
  
  // 分配律 a * (b + c) = a * b + a * c (在安全范围内)
  let a = 10
  let b = 20
  let c = 30
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "complex_arithmetic_sequences_test" {
  // 复杂数学序列测试
  // 阶乘-like 计算: 1*2 + 2*3 + 3*4
  let term1 = multiply(1, 2)
  let term2 = multiply(2, 3)
  let term3 = multiply(3, 4)
  let sum = add(add(term1, term2), term3)
  assert_eq(20, sum)
  
  // 几何级数: 2 + 2*2 + 2*2*2
  let geo1 = 2
  let geo2 = multiply(2, 2)
  let geo3 = multiply(multiply(2, 2), 2)
  let geo_sum = add(add(geo1, geo2), geo3)
  assert_eq(14, geo_sum)
  
  // 复合运算: (a + b) * (c + d)
  let a = 5
  let b = 3
  let c = 7
  let d = 2
  let sum1 = add(a, b)
  let sum2 = add(c, d)
  let result = multiply(sum1, sum2)
  assert_eq(80, result)
}

test "boundary_conditions_test" {
  // 边界条件测试
  // 大数值运算（在安全范围内）
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000, multiply(large_num, 1))  // 修改为避免溢出
  
  // 接近极值的运算
  let near_max = 2147483640  // 接近 Int32 最大值
  let near_min = -2147483640 // 接近 Int32 最小值
  
  assert_eq(2147483647, add(near_max, 7))  // 应该饱和到最大值
  assert_eq(-2147483648, add(near_min, -8)) // 应该饱和到最小值
  
  // 除法的边界情况
  assert_eq(1, divide_with_ceil(1, 2147483647))
  assert_eq(0, divide_with_ceil(0, 2147483647))
  assert_eq(-1, divide_with_ceil(-1, 2147483647))
}

test "assert_functions_comprehensive_test" {
  // 断言函数综合测试
  // assert_true 测试
  assert_true(true)
  assert_true(1 == 1)
  assert_true(0 == 0)
  assert_true(-1 == -1)
  assert_true(100 > 50)
  assert_true(-100 < -50)
  
  // assert_false 测试
  assert_false(false)
  assert_false(1 == 0)
  assert_false(0 == 1)
  assert_false(-1 == 1)
  assert_false(100 < 50)
  assert_false(-100 > -50)
  
  // assert_eq 测试
  assert_eq(0, 0)
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq(2147483647, 2147483647)
  assert_eq(-2147483648, -2147483648)
  
  // assert_eq_string 测试
  assert_eq_string("", "")
  assert_eq_string("test", "test")
  assert_eq_string("Hello, World!", "Hello, World!")
  assert_eq_string("12345", "12345")
  assert_eq_string("!@#$%", "!@#$%")
}

test "real_world_scenarios_test" {
  // 实际应用场景测试
  // 场景1: 计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 29
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(626, total)
  
  // 场景2: 计算平均分数（向上取整）
  let math_score = 85
  let english_score = 92
  let science_score = 78
  let history_score = 88
  let total_score = add(add(add(math_score, english_score), science_score), history_score)
  let average = divide_with_ceil(total_score, 4)
  assert_eq(87, average)
  
  // 场景3: 批量处理任务数计算
  let tasks_per_batch = 25
  let total_tasks = 123
  let batches_needed = divide_with_ceil(total_tasks, tasks_per_batch)
  assert_eq(5, batches_needed)
  
  // 场景4: 用户欢迎消息生成
  let user_name = "李明"
  let welcome_message = greet(user_name)
  assert_eq_string("Hello, 李明!", welcome_message)
}