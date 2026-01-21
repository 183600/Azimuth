// 新的综合测试用例 - 标准MoonBit测试语法
// 测试azimuth包的核心功能

test "add_function_comprehensive" {
  // 基本加法测试
  assert_eq(10, add(3, 7))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, add(max_val, 1))  // 溢出保护
  assert_eq(min_val, add(min_val, -1)) // 溢出保护
  
  // 负数测试
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
  
  // 零值测试
  assert_eq(100, add(100, 0))
  assert_eq(0, add(0, 0))
}

test "multiply_function_comprehensive" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  
  // 零值测试
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  
  // 单位元测试
  assert_eq(100, multiply(100, 1))
  assert_eq(100, multiply(1, 100))
  
  // 负数测试
  assert_eq(-20, multiply(4, -5))
  assert_eq(20, multiply(-4, -5))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, -1))  // 特殊情况处理
}

test "greet_function_comprehensive" {
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  
  // 空字符串测试
  assert_eq_string("Hello, !", greet(""))
  
  // 特殊字符测试
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  
  // Unicode字符测试
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  
  // 长字符串测试
  let long_name = "VeryLongNameForTestingPurposes"
  assert_eq_string("Hello, VeryLongNameForTestingPurposes!", greet(long_name))
}

test "mathematical_properties" {
  // 加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  let c = 42
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_scenarios" {
  // 购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let total = add(subtotal1, subtotal2)
  
  assert_eq(697, total)  // 199*2 + 299*1 = 398 + 299 = 697
  
  // 温度转换（简化版：F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25*2 + 30 = 80
  
  // 面积计算
  let length = 12
  let width = 8
  let area = multiply(length, width)
  assert_eq(96, area)
}

test "edge_cases_and_special_values" {
  // 最小值和最大值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(-1, add(max_val, min_val))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  
  // 大数运算
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 连续运算
  let result = add(multiply(5, 3), multiply(2, 4))
  assert_eq(23, result)  // 5*3 + 2*4 = 15 + 8 = 23
}

test "string_processing_edge_cases" {
  // 数字字符串
  assert_eq_string("Hello, 123!", greet("123"))
  
  // 混合字符
  assert_eq_string("Hello, user123!", greet("user123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  
  // 空格处理
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  
  // 特殊Unicode字符
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, éñçödë!", greet("éñçödë"))
}

test "complex_calculation_scenarios" {
  // 复利计算（简化版）
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000
  
  // 平均值计算
  let scores = [90, 85, 95, 80, 100]
  let sum = add(add(add(add(90, 85), 95), 80), 100)
  let average = sum / 5
  assert_eq(90, average)
  
  // 阶乘计算（5!）
  let factorial = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial)
}

test "assertion_functions_test" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 复杂断言
  let result = add(10, 20)
  assert_true(result == 30)
  assert_false(result != 30)
  assert_eq(30, result)
  
  let greeting = greet("Test")
  assert_true(greeting == "Hello, Test!")
  assert_eq_string("Hello, Test!", greeting)
}

test "performance_and_stress" {
  // 大数连续运算
  let large_num = 1000000
  let result1 = add(large_num, add(large_num, large_num))
  assert_eq(3000000, result1)
  
  let result2 = multiply(1000, multiply(1000, 1000))
  assert_eq(1000000000, result2)
  
  // 嵌套运算
  let nested = add(multiply(10, add(5, 3)), multiply(4, add(2, 6)))
  assert_eq(108, nested)  // 10*(5+3) + 4*(2+6) = 10*8 + 4*8 = 80 + 32 = 112
  
  // 字符串处理性能
  let names = ["Alice", "Bob", "Charlie", "David", "Eve"]
  let greeting1 = greet(names[0])
  let greeting2 = greet(names[2])
  let greeting3 = greet(names[4])
  
  assert_eq_string("Hello, Alice!", greeting1)
  assert_eq_string("Hello, Charlie!", greeting2)
  assert_eq_string("Hello, Eve!", greeting3)
}