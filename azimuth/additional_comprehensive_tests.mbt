// 额外的综合测试用例
// 测试 azimuth 包的高级功能和边界情况

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法功能
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(3, 3))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  
  // 负除数
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(2, divide_with_ceil(-6, -3))
  
  // 零相关
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -3))
}

test "add_function_extreme_cases" {
  // 测试加法函数的极端情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近边界的加法
  assert_eq(max_val, add(max_val - 100, 100))
  assert_eq(max_val, add(max_val - 1000, 1000))
  assert_eq(min_val, add(min_val + 100, -100))
  assert_eq(min_val, add(min_val + 1000, -1000))
  
  // 大数加法
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(0, add(1000000, -1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
}

test "multiply_function_sqrt_boundary" {
  // 测试乘法函数的平方根边界
  let sqrt_max = 46340  // sqrt(2147483647) 的整数部分
  
  // 平方根边界测试
  assert_eq(2147395600, multiply(sqrt_max, sqrt_max))
  assert_eq(2147483647, multiply(sqrt_max, sqrt_max + 1))
  assert_eq(-2147395600, multiply(sqrt_max, -sqrt_max))
  assert_eq(-2147483647, multiply(sqrt_max, -(sqrt_max + 1)))
  
  // 大数乘法
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(-100000000, multiply(10000, -10000))
}

test "greet_function_unicode_and_special_chars" {
  // 测试问候函数的Unicode和特殊字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🚀🛸!", greet("🚀🛸"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // 嵌套运算
  assert_eq(25, add(multiply(3, 5), multiply(2, 5)))
  assert_eq(20, multiply(add(3, 2), 4))
  assert_eq(8, divide_with_ceil(add(multiply(2, 3), 2), 2))
  
  // 多层嵌套
  assert_eq(15, add(multiply(divide_with_ceil(10, 2), 2), 5))
  assert_eq(12, multiply(add(divide_with_ceil(7, 3), 2), 3))
  
  // 复杂表达式
  assert_eq(16, add(multiply(2, 5), divide_with_ceil(10, 3)))
  assert_eq(9, add(multiply(divide_with_ceil(10, 3), 2), 1))
}

test "assertion_functions_various_types" {
  // 测试断言函数的各种类型
  // 布尔断言
  assert_true(1 == 1)
  assert_true(0 == 0)
  assert_true(true)
  assert_false(1 == 2)
  assert_false(false)
  
  // 整数断言
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq(0, 0)
  assert_eq(2147483647, 2147483647)
  assert_eq(-2147483648, -2147483648)
  
  // 字符串断言
  assert_eq_string("", "")
  assert_eq_string("test", "test")
  assert_eq_string("Hello, World!", "Hello, World!")
  assert_eq_string("Unicode: 测试", "Unicode: 测试")
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1 = 199
  let item2 = 299
  let item3 = 99
  let subtotal = add(add(item1, item2), item3)
  let tax_rate = 3  // 1/3 税率
  let total = multiply(subtotal, tax_rate)
  assert_eq(1791, total)
  
  // 场景2：平均分计算（向上取整）
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 88
  let total_score = add(add(add(score1, score2), score3), score4)
  let average = divide_with_ceil(total_score, 4)
  assert_eq(87, average)
  
  // 场景3：用户欢迎消息
  let username = "张三"
  let welcome = greet(username)
  assert_eq_string("Hello, 张三!", welcome)
}

test "performance_critical_operations" {
  // 测试性能关键操作
  let base = 1000
  
  // 连续操作
  let result1 = add(add(add(base, 500), 250), 125)
  let result2 = multiply(multiply(base, 2), 3)
  let result3 = divide_with_ceil(multiply(base, 7), 3)
  
  assert_eq(1875, result1)
  assert_eq(6000, result2)
  assert_eq(2334, result3)
  
  // 复杂计算链
  let chain_result = add(multiply(divide_with_ceil(add(base, 100), 5), 3), 50)
  assert_eq(710, chain_result)
}

test "edge_case_combinations" {
  // 测试边界情况组合
  let max_val = 2147483647
  let min_val = -2147483648
  let sqrt_max = 46340
  
  // 边界值组合运算
  assert_eq(max_val - 1, add(max_val, min_val))
  assert_eq(min_val + 1, add(min_val, max_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 平方根边界与除法组合
  let sqrt_result = multiply(sqrt_max, sqrt_max)
  let div_result = divide_with_ceil(sqrt_result, 1000)
  assert_eq(2147396, div_result)
  
  // 极值字符串处理
  let empty_greet = greet("")
  let single_char = greet("A")
  let long_string = greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  
  assert_eq_string("Hello, !", empty_greet)
  assert_eq_string("Hello, A!", single_char)
  assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", long_string)
}