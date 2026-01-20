// Azimuth 项目标准核心测试用例
// 专注于测试核心功能和边界情况

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  // 正常情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  
  // 整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(21, 3))
  
  // 较小数字
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 被除数为0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  
  // 除数为1
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(7, 1))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(100, 1))
  
  // 接近整除的情况
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(6, 4))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数处理
  // 负被除数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  
  // 负除数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(11, -3))
  
  // 双负数
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
}

test "add_function_zero_handling" {
  // 测试加法函数的零值处理
  let max_val = 2147483647
  let min_val = -2147483648
  let normal_val = 42
  
  // 与0相加
  @azimuth.assert_eq(normal_val, @azimuth.add(normal_val, 0))
  @azimuth.assert_eq(normal_val, @azimuth.add(0, normal_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 负零处理（实际上就是0）
  @azimuth.assert_eq(normal_val, @azimuth.add(normal_val, -0))
}

test "multiply_function_special_cases" {
  // 测试乘法函数的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  let normal_val = 123
  
  // 与0相乘
  @azimuth.assert_eq(0, @azimuth.multiply(normal_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, normal_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 与1相乘
  @azimuth.assert_eq(normal_val, @azimuth.multiply(normal_val, 1))
  @azimuth.assert_eq(normal_val, @azimuth.multiply(1, normal_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  
  // 与-1相乘
  @azimuth.assert_eq(-normal_val, @azimuth.multiply(normal_val, -1))
  @azimuth.assert_eq(-normal_val, @azimuth.multiply(-1, normal_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 特殊情况：最小值乘以-1
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  // 基本测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  
  // 单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, 1!", @azimuth.greet("1"))
}

test "combined_operations_complex_scenarios" {
  // 测试组合运算的复杂场景
  // 场景：计算 (a + b) * c - d
  let a = 15
  let b = 25
  let c = 3
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)  // 40
  let product = @azimuth.multiply(sum_ab, c)  // 120
  let result = @azimuth.add(product, -d)  // 100
  
  @azimuth.assert_eq(100, result)
  
  // 场景：使用向上取整除法的复杂计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)  // 4
  
  let cost_per_box = 15
  let total_cost = @azimuth.multiply(boxes_needed, cost_per_box)  // 60
  
  @azimuth.assert_eq(4, boxes_needed)
  @azimuth.assert_eq(60, total_cost)
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 计算等差数列前n项和：1 + 2 + 3 + ... + n = n*(n+1)/2
  let n = 10
  let n_plus_1 = @azimuth.add(n, 1)  // 11
  let product = @azimuth.multiply(n, n_plus_1)  // 110
  let sum = product / 2  // 55
  
  @azimuth.assert_eq(55, sum)
  
  // 计算等比数列前n项和：1 + 2 + 4 + 8 + ... + 2^(n-1) = 2^n - 1
  // 手动计算前4项：1 + 2 + 4 + 8 = 15
  let term1 = 1
  let term2 = @azimuth.multiply(term1, 2)  // 2
  let term3 = @azimuth.multiply(term2, 2)  // 4
  let term4 = @azimuth.multiply(term3, 2)  // 8
  
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(15, geometric_sum)
}

test "assertion_functions_behavior" {
  // 测试断言函数的行为
  // 这些测试主要验证断言函数在正确输入时不会抛出异常
  
  // 测试 assert_eq
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(-100, -100)
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  
  // 测试 assert_eq_string
  @azimuth.assert_eq_string("hello", "hello")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("123", "123")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  
  // 测试 assert_true
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(42 > 0)
  @azimuth.assert_true(-10 < 0)
  
  // 测试 assert_false
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(42 < 0)
  @azimuth.assert_false(-10 > 0)
}

test "boundary_overflow_comprehensive" {
  // 测试边界溢出的综合情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 乘法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值的正常运算
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}