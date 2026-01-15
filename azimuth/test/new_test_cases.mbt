// 新增的 MoonBit 测试用例
// 测试 azimuth 库的核心功能
import azimuth

test "add_function_zero_handling" {
  // 测试 add 函数的零值处理
  assert_eq(42, add(0, 42))
  assert_eq(42, add(42, 0))
  assert_eq(0, add(0, 0))
  assert_eq(-17, add(0, -17))
  assert_eq(-17, add(-17, 0))
}

test "add_function_negative_values" {
  // 测试 add 函数的负数处理
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(5, add(8, -3))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-25, 25))
}

test "multiply_function_by_zero_and_one" {
  // 测试 multiply 函数的0和1乘法
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(123, 0))
  assert_eq(0, multiply(0, -456))
  assert_eq(123, multiply(1, 123))
  assert_eq(123, multiply(123, 1))
  assert_eq(-456, multiply(1, -456))
  assert_eq(-456, multiply(-456, 1))
}

test "multiply_function_negative_numbers" {
  // 测试 multiply 函数的负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(100, multiply(-10, -10))
  assert_eq(-100, multiply(10, -10))
}

test "greet_function_special_characters" {
  // 测试 greet 函数的特殊字符处理
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, &*()!", greet("&*()"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
}

test "combined_operations_test" {
  // 测试组合运算
  let a = 5
  let b = 3
  let c = 2
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
  
  // 测试 (a + c) * (b + c)
  let result3 = multiply(add(a, c), add(b, c))
  assert_eq(35, result3)
}

test "boundary_values_test" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 测试接近边界的乘法
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
}

test "string_empty_and_whitespace" {
  // 测试空字符串和空白字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, !", greet(" "))
  assert_eq_string("Hello,    !", greet("    "))
  assert_eq_string("Hello, \t!", greet("\t"))
  assert_eq_string("Hello, \n!", greet("\n"))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = 17
  
  // 加法恒等式
  assert_eq(add(x, y), add(y, x))  // 交换律
  assert_eq(add(add(x, y), 0), add(x, y))  // 零元素
  
  // 乘法恒等式
  assert_eq(multiply(x, y), multiply(y, x))  // 交换律
  assert_eq(multiply(multiply(x, y), 1), multiply(x, y))  // 单位元素
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 100
  
  let subtotal = add(multiply(item1_price, item1_quantity), 
                    multiply(item2_price, item2_quantity))
  let total = add(subtotal, -discount)
  
  assert_eq(597, total)
  
  // 计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let sum_scores = add(add(score1, score2), score3)
  let average = sum_scores / 3
  
  assert_eq(90, average)
}