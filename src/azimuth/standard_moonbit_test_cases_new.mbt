// 标准 MoonBit 测试用例
// 为 azimuth 项目新增的高质量测试用例

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1, multiply(1, 1))
  assert_eq(10000, multiply(100, 100))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(10, 0))
  assert_eq(-10, subtract(0, 10))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
  assert_eq(0, subtract(-10, -10))
}

test "divide_with_ceil_function_precision" {
  // 测试向上取整除法函数的精度
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_various_scenarios" {
  // 测试问候函数的各种场景
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let a = 12
  let b = 8
  let c = 5
  
  // 复合运算测试
  let result1 = add(multiply(a, b), c)  // 12 * 8 + 5 = 101
  let result2 = multiply(add(a, b), c)  // (12 + 8) * 5 = 100
  let result3 = subtract(multiply(a, c), b)  // 12 * 5 - 8 = 52
  
  assert_eq(101, result1)
  assert_eq(100, result2)
  assert_eq(52, result3)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 15
  let y = 7
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), 3), add(x, add(y, 3)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
  
  // 分配律
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "boundary_value_analysis_comprehensive" {
  // 综合边界值分析
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, subtract(5, 0))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 负单位值测试
  assert_eq(4, add(5, -1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(6, subtract(5, -1))
  assert_eq(-5, divide_with_ceil(-5, 1))
}

test "real_world_application_test" {
  // 真实世界应用测试：购物车计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 8
  let discount = 10
  
  // 计算小计
  let subtotal = multiply(item_price, quantity)  // 25 * 4 = 100
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)  // ceil(100 * 8 / 100) = 8
  
  // 计算折扣
  let discount_amount = divide_with_ceil(multiply(subtotal, discount), 100)  // ceil(100 * 10 / 100) = 10
  
  // 计算总价
  let total = subtract(add(subtotal, tax_amount), discount_amount)  // 100 + 8 - 10 = 98
  
  assert_eq(100, subtotal)
  assert_eq(8, tax_amount)
  assert_eq(10, discount_amount)
  assert_eq(98, total)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(50, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(33334, divide_with_ceil(large_num, 3))
  
  // 极端精度测试
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  
  // 字符串极端情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
}