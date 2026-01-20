// 标准 MoonBit 测试用例 - azimuth 包增强测试
// 测试核心功能的各种边界情况和实际应用场景

test "add_function_comprehensive" {
  // 加法函数综合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 基本加法
  assert_eq(0, add(0, 0))
  assert_eq(10, add(7, 3))
  assert_eq(-10, add(-7, -3))
  assert_eq(0, add(5, -5))
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, add(max_val - 100, 100))
  assert_eq(min_val, add(min_val + 100, -100))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 基本乘法
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
  
  // 特殊值乘法
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(-1, min_val))
}

test "greet_function_various_inputs" {
  // greet 函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello,   !", greet("   "))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(1, 3))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  
  // 零相关
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "complex_mathematical_operations" {
  // 复杂数学运算测试
  // 嵌套运算
  assert_eq(25, add(multiply(3, 5), multiply(2, 5)))
  assert_eq(20, multiply(add(3, 2), 4))
  assert_eq(8, divide_with_ceil(add(multiply(2, 3), 2), 2))
  
  // 多层嵌套
  assert_eq(15, add(multiply(divide_with_ceil(10, 2), 2), 5))
  assert_eq(12, multiply(add(divide_with_ceil(7, 3), 2), 3))
}

test "boundary_value_operations" {
  // 边界值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值组合运算
  assert_eq(max_val, add(max_val - 1, 1))
  assert_eq(min_val, add(min_val + 1, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 接近边界的运算
  assert_eq(2147483647, add(2147483647 - 1000, 1000))
  assert_eq(-2147483648, add(-2147483648 + 1000, -1000))
}

test "assertion_functions_validation" {
  // 断言函数验证测试
  // 布尔断言
  assert_true(true)
  assert_false(false)
  assert_true(1 == 1)
  assert_false(1 == 2)
  
  // 整数断言
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq(0, 0)
  
  // 字符串断言
  assert_eq_string("test", "test")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", "Hello, World!")
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 场景1：计算购物车总价（含税向上取整）
  let item1_price = 99
  let item2_price = 149
  let tax_rate = 3  // 1/3 的税率
  let subtotal = add(item1_price, item2_price)
  let total_with_tax = multiply(subtotal, tax_rate)
  assert_eq(744, total_with_tax)
  
  // 场景2：平均分计算（向上取整）
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let total_score = add(add(score1, score2), score3)
  let average_score = divide_with_ceil(total_score, 3)
  assert_eq(86, average_score)
  
  // 场景3：用户欢迎消息
  let username = "张三"
  let welcome_msg = greet(username)
  assert_eq_string("Hello, 张三!", welcome_msg)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出情况测试
  let overflow_add = add(max_val, max_val)
  let overflow_mul = multiply(max_val, 2)
  assert_true(overflow_add == max_val || overflow_add == min_val)
  assert_true(overflow_mul == max_val || overflow_mul == min_val)
  
  // 特殊字符串处理
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \"!", greet("\""))
  assert_eq_string("Hello, \\", greet("\\"))
}